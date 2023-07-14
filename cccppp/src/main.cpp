#include <sstream>
#include <string>

#include "clang/AST/AST.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ParentMapContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CommonOptionsParser.h" // TODO: remove
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Program.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;
using clang::DynTypedNode;

class CCCPPPASTVisitor : public RecursiveASTVisitor<CCCPPPASTVisitor> {
public:
  bool shouldTraversePostOrder() const /* override */ { return true; }
  CCCPPPASTVisitor(Rewriter &R, ASTContext &C) : TheRewriter(R), TheContext(C) {}

  /// helper that gets the original text for any expression (FIXME: can do more efficiently?)
  std::string GetReplacedText(Stmt *e)
  {
    /* CARE: nested rewritings!
     * If we replace some text, then replace some text that includes some replaced text,
     * we want to pick up the replaced version. The rewriter should be able to give us
     * this, probably. Instead of getting the character data from a memory buf, we
     * want to read from the rewrite rope. */
    std::string s = TheRewriter.getRewrittenText(e->getSourceRange());
    // in our particular case, it should not end with a comma!
    assert(s.at(s.length() - 1) != ',');
    return s;
  }
  
  void ReplaceExpression(Expr *e, const std::string& replacement)
  {
    llvm::errs() << "Replacing expression having current text: `"
      << TheRewriter.getRewrittenText(e->getSourceRange()) << "'\n";
    llvm::errs() << "Replacement text is: `" << replacement << "'\n";
    auto& sm = TheRewriter.getSourceMgr();
    const char *start = sm.getCharacterData(e->getSourceRange().getBegin());
    std::pair<FileID, unsigned> locStart = sm.getDecomposedLoc(e->getSourceRange().getBegin());
    std::pair<FileID, unsigned> locEnd = sm.getDecomposedLoc(e->getSourceRange().getEnd());
    int len = 1 + locEnd.second - locStart.second;
    llvm::errs() << "Original text was (maybe): " << std::string(start, (len < 0) ? 0 : (size_t) len)
      << "\n";
    TheRewriter.ReplaceText(e->getSourceRange(), replacement);
    llvm::errs() << "Now the expression has text : `"
      << GetReplacedText(e) << "'\n";
    // assert that it really has the effect we want
    assert(GetReplacedText(e) == replacement);
  }
  unsigned SameFileDisplacement(const SourceLocation& start, const SourceLocation &end)
  {
    auto& sm = TheRewriter.getSourceMgr();
    std::pair<FileID, unsigned> locStart = sm.getDecomposedLoc(start);
    std::pair<FileID, unsigned> locEnd = sm.getDecomposedLoc(end);
    assert(locStart.first == locEnd.first);
    return locEnd.second - locStart.second;
  }
  void ReplaceBinaryExpression(Expr *eOuter, Expr *eSubLeft, Expr *eSubRight,
    const std::string& replacementA, const std::string& replacementB,
    const std::string& replacementC
  )
  {
    /* PROBLEM:
       If we have
            expr1  [ expr2 ]
           ^^    ^   ^    ^ ^
           |`----'    `---' |
           \________________/
            sourcerange

       and we rewrite it to
            __primop_blah( expr1 , expr2  )
            ^             ^     ^ ^     ^  ^
            |             `-----' `-----'  |
            `------------------------------'
       clearly sourcerange(file/offset..file/offset)) now maps to the rewritten buffer
              
       BUT do the expr1's and expr2's source ranges also point there?
       How does Clang know to map *their* locations into the rewrite buffer?
       It doesn't!
       Clang also doesn't know that the expression's AST has changed.
       Indeed it hasn't! Clang still sees an ArraySubscriptExpr, say.
       We have just updated the pointers in the text buffers.

       It took some trial and error to get something that works. Here is the idea.
       In short, we never replace the contents of e1 and e2.
       Instead we rewrite the "glue syntax" of the overarching expression

             AAAAA        BBBBB        CCCCCC
                     p      [     off     ]
             XXXXX __e1__ YYYYY __e2__ ZZZZZZ
             `------------eOuter------------'
       That means instead of one big replace,
       - replace the AAAAA with XXXXX, i.e. from the start of eOuter up to the start of e1
       - replace the BBBBB with YYYYY
       - replace the CCCCC with ZZZZZ
     */
    llvm::errs() << "Replacing binary expression having current text: `"
      << TheRewriter.getRewrittenText(eOuter->getSourceRange()) << "'\n";
    llvm::errs() << "Left-hand subexpression is:  `"
      << TheRewriter.getRewrittenText(eSubLeft->getSourceRange()) << "'\n";
    llvm::errs() << "Right-hand subexpression is:  `"
      << TheRewriter.getRewrittenText(eSubRight->getSourceRange()) << "'\n";
    llvm::errs() << "Replacement fragments: {`" << replacementA << "'}, {`"
      << replacementB << "'}, {`"
      << replacementC << "'}\n";
    auto& sm = TheRewriter.getSourceMgr();
    auto outerBeginLoc = sm.getDecomposedLoc(eOuter->getSourceRange().getBegin());
    unsigned offsetBeginOuter = outerBeginLoc.second;
    unsigned offsetBeginSubLeft = sm.getDecomposedLoc(eSubLeft->getSourceRange().getBegin()).second;
    unsigned offsetBeginSubRight = sm.getDecomposedLoc(eSubRight->getSourceRange().getBegin()).second;;
    // these are wrong!!! Is the moral that we should not ask the source manager for any offset?
    auto outerEndLoc = sm.getDecomposedLoc(eOuter->getSourceRange().getEnd());
    unsigned offsetEndOuter = outerEndLoc.second;
    unsigned offsetEndSubLeft = sm.getDecomposedLoc(eSubLeft->getSourceRange().getEnd()).second;
    unsigned offsetEndSubRight = sm.getDecomposedLoc(eSubRight->getSourceRange().getEnd()).second;;

    auto inserted = rawRangesRewritten.insert(std::make_pair(outerBeginLoc.first,
      std::make_pair(outerBeginLoc.second, outerEndLoc.second)));
    if (!inserted.second)
    {
      // we've been here before.
      llvm::errs() << "Skipping rewriting as we've processed this source range before (FIXME: skip those extra template instantiations...)\n";
      return;
    }
    /* We may be rewriting something that was rewritten earlier.
     * So some hackery is necessary when calculating the offsets
     * and lengths of replacement text. We always want to calculate
     * based on the *rewritten* text length. */
    unsigned offsetHackedEndOuter = offsetBeginOuter
        + TheRewriter.getRewrittenText(eOuter->getSourceRange()).length();
    unsigned offsetHackedEndSubLeft = offsetBeginSubLeft
        + TheRewriter.getRewrittenText(eSubLeft->getSourceRange()).length();
    unsigned offsetHackedEndSubRight = offsetBeginSubRight
        + TheRewriter.getRewrittenText(eSubRight->getSourceRange()).length();
    /* FIXME: should we also have an offsetHackedBegin{Outer,Subleft,Subright}?
     * i.e. we always want to calculate lengths in terms of rewritten text.
     * Can we calculate begin offsets in those terms too?
     * i.e. can we use the rewriter's view more cleanly/consistently? */
    unsigned lengthA = offsetBeginSubLeft - offsetBeginOuter;
    unsigned lengthB = offsetBeginSubRight - offsetHackedEndSubLeft;
    /* If lengthB is negative (huge), it means the (hacked) left-hand expression ends *after*
     * the right-hand one begins, i.e. rewriting extended the left-hand expression by more than
     * the original distance between LH-end and RH-begin. FIXME: can this still happen? */
    unsigned lengthC = offsetHackedEndOuter - offsetHackedEndSubRight;
    static const unsigned MAXIMUM_SANE_LENGTH = 10000;
    llvm::errs() << "Replacee fragments have lengths " << lengthA << ", " << lengthB << ", "
      << lengthC << "\n";
    assert(lengthA < MAXIMUM_SANE_LENGTH);
    assert(lengthB < MAXIMUM_SANE_LENGTH);
    assert(lengthC < MAXIMUM_SANE_LENGTH);
    llvm::errs() << "Expressions (outer, l, r) have file begin offsets " << offsetBeginOuter
        << ", " << offsetBeginSubLeft << ", " << offsetBeginSubRight << "\n";
    llvm::errs() << "Expressions (outer, l, r) have file end offsets " << offsetEndOuter
        << ", " << offsetEndSubLeft << ", " << offsetEndSubRight << "\n";
    llvm::errs() << "Expressions (outer, l, r) have hacked file end offsets " 
        << offsetHackedEndOuter
        << ", " << offsetHackedEndSubLeft << ", " << offsetHackedEndSubRight << "\n";
    // FIXME: does this do anything? It seems dead.
    Rewriter::RewriteOptions notBegin;
    notBegin.IncludeInsertsAtBeginOfRange = false;
    /* Recall our picture:
             AAAAA        BBBBB        CCCCCC
                     p      [     off     ]
             XXXXX __e1__ YYYYY __e2__ ZZZZZZ
             `------------eOuter------------'
     */
    if (lengthA > 0) TheRewriter.RemoveText(eOuter->getSourceRange().getBegin(),
      lengthA);
    // inserting before the beginning of the left range does not do not what we want;
    //   the inserted text is included in the left expression's range after the rewrite,
    // i.e. it's more like "at the beginning" than "before the beginning".
    // try inserting "after" eOuter's getSourceRange().getBegin()?
    // No, that does the same thing
    // Try inserting "not after", i.e. passing "false" to the 3-args InsertText?
    // No, that does the same thing too. What about?
    //TheRewriter.ReplaceText(eOuter->getSourceRange().getBegin(),
    //  lengthA,
    //  replacementA);
    // No, that turned "__a" into "__m" i.e. remapped the left subexpr's start to the replacement
    // (because here the left subexpression begins at the same place as the outer expression).
    // What about doing the insertion *then* doing the removal?
    // Using InsertTextBefore, this seems to do the right thing.
    TheRewriter.InsertTextBefore(eSubLeft->getSourceRange().getBegin(),
      replacementA);
    TheRewriter.ReplaceText(
      eSubLeft->getSourceRange().getBegin()
        .getLocWithOffset(offsetHackedEndSubLeft - offsetBeginSubLeft),
      lengthB,
      replacementB);
    TheRewriter.ReplaceText(
      eSubRight->getSourceRange().getBegin()
        .getLocWithOffset(offsetHackedEndSubRight - offsetBeginSubRight),
      lengthC,
      replacementC);
    llvm::errs() << "Now left-hand subexpression is:  `"
      << TheRewriter.getRewrittenText(eSubLeft->getSourceRange()) << "'\n";
    llvm::errs() << "Now right-hand subexpression is:  `"
      << TheRewriter.getRewrittenText(eSubRight->getSourceRange()) << "'\n";
    llvm::errs() << "Now whole binary expression is: `"
      << TheRewriter.getRewrittenText(eOuter->getSourceRange()) << "'\n";
  }
  
  /* For now, let's visit expressions that involve operator[] or its builtin
   * (we say "primop"). These are (in reverse order) 
   * - ArraySubscriptExpr,
   * - CXXOperatorCallExpr where the first operand is an ImplicitCastExpr of a DeclRefExpr
   *    ref'ing the decl of the operator being called, i.e. an operator[].
   * In Clang, expressions (Expr) are a kind of statement (Stmt).
   */
  bool VisitStmt(Stmt *s) {
    /* We have to look at the AST context to decide whether to skip this. 
     * We skip if
     * (1) it's a template instance when we have already rewritten the template
     *        definition;
     * (2) we're in a context like addr-of, sizeof or decltype where the code
     *        is not actually being executed.
     */
    auto parents = TheContext.getParents(*s);
    while (!parents.empty())
    {
      const DynTypedNode *parentNode;
      switch (parents.size())
      {
        case 0: assert(false);
        case 1: // good
          parentNode = &parents[0];
          break;
        default:
          llvm::errs() << "More than one parent for ";
          s->printPretty(llvm::errs(), nullptr, PrintingPolicy(LangOptions()));
          llvm::errs() << " so just taking the first one (FIXME).\n";
          parentNode = &parents[0];
          break;
      }
      /* Now we have a parent. We might have seen enough.
       * If we hit a decl, that's too far. */
      bool shouldStopWalkingParents = [&]() -> bool {
        const clang::Decl *aDecl = parentNode->get<clang::Decl>();
        return !!aDecl;
      }();
      if (shouldStopWalkingParents) break;
      bool shouldSkipGivenThisAncestor = [&]() -> bool {
        const clang::Stmt *aStmt = parentNode->get<clang::Stmt>();
        if (aStmt && (
            (isa<UnaryOperator>(aStmt) && cast<clang::UnaryOperator>(aStmt)->getOpcode() == UO_AddrOf)
          ))
        {
          llvm::errs() << "Skipping something under an addr-of expr!\n";
          return true;
        } else if (aStmt && (
           (isa<UnaryExprOrTypeTraitExpr>(aStmt))
          ))
        {
          llvm::errs() << "Skipping something under a sizeof, _Alignof, decltype or similar expr!\n";
          return true;
        }
        return false;
      }();
      
      if (shouldSkipGivenThisAncestor)
      {
        return true;
      }
      parents = TheContext.getParents(*parentNode);
    }
    if (isa<ArraySubscriptExpr>(s)) {
      /* There are two cases:
       * (1) it's really the builtin array subscript;
       * (2) it's a template that might actually bind to an overload.
       */
      ArraySubscriptExpr *e = cast<ArraySubscriptExpr>(s);
      llvm::errs() << "Post-order-reached a new ArraySubscriptExpr: ";
      e->printPretty(llvm::errs(), nullptr, PrintingPolicy(LangOptions()));
      llvm::errs() << " at ";
      e->getSourceRange().getBegin().print(llvm::errs(), TheRewriter.getSourceMgr());
      llvm::errs() << "\n";
      e->dump();
      // replace it with some text we have crafted
      QualType indexedType = e->getLHS()->getType();
      // is this definitely an array?
      if (indexedType.getTypePtr()->isTemplateTypeParmType()
        || indexedType.getTypePtr()->isDependentType()
        || indexedType.getTypePtr()->isInstantiationDependentType()
        || indexedType.getTypePtr()->isUndeducedType())
      {
        /* We want to print the type name, or an expression for it,
         * as it already appears in the code. But in many cases this
         * comes out as "<dependent type>", and I haven't figured out a
         * way to make clang print what we want. So use decltype() for now. */
        std::string lhBefore = GetReplacedText(e->getLHS());
        std::string rhBefore = GetReplacedText(e->getRHS());
        ReplaceBinaryExpression(e, e->getLHS(), e->getRHS(),
          std::string("__maybe_primop_subscript<")
          + "decltype("
          + lhBefore
          + "), !__has_subscript_overload<decltype(" + lhBefore + ")>::value>()(",
          ", ",
          ")"
        );
        // after replacement, we should still have the same view of the subexpressions
        assert(e->getLHS()->getSourceRange().getBegin() == e->getSourceRange().getBegin()
        || lhBefore == GetReplacedText(e->getLHS()));
        assert(rhBefore == GetReplacedText(e->getRHS()));
      }
      else
      {
        std::string lhBefore = GetReplacedText(e->getLHS());
        std::string rhBefore = GetReplacedText(e->getRHS());
        ReplaceBinaryExpression(e, e->getLHS(), e->getRHS(),
          std::string("__primop_subscript<")
          + indexedType.getAsString()
          + " >()(",
          ", ",
          ")"
        );
        // after replacement, we should still have the same view of the subexpressions
        // EXCEPT we can't do this if the LHS begins at the same place as the
        // outer expression
        assert(e->getLHS()->getSourceRange().getBegin() == e->getSourceRange().getBegin()
        || lhBefore == GetReplacedText(e->getLHS()));
        assert(rhBefore == GetReplacedText(e->getRHS()));
      }
    }
    return true;
  }

private:
  Rewriter &TheRewriter;
  ASTContext &TheContext;
  std::set<std::pair<FileID, std::pair<unsigned, unsigned>>> rawRangesRewritten;
};

// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser.
class CCCPPPConsumer : public ASTConsumer {
public:
  CCCPPPConsumer(Rewriter &R, ASTContext &C) : Visitor(R, C), R(R), C(C)
  {
    // If I do this
    // C.getParents(*C.getTranslationUnitDecl());
    // ... I get no parent info at all.
    // If I don't do it, I get it for the first top-level decl, and not after that.
    // UNLESS... I re-set the traversal scope each time around the loop in HandleTopLevelDecl.
    llvm::errs() << "Translation unit decl has source range: begin ";
    C.getTranslationUnitDecl()->getSourceRange().getBegin().print(llvm::errs(), R.getSourceMgr());
    llvm::errs() << ", end ";
    C.getTranslationUnitDecl()->getSourceRange().getEnd().print(llvm::errs(), R.getSourceMgr());
    llvm::errs() << "\n";
    llvm::errs() << "Main file has source range: begin ";
    C.getTranslationUnitDecl()->getSourceRange().getBegin().print(llvm::errs(), R.getSourceMgr());
    llvm::errs() << ", end ";
    C.getTranslationUnitDecl()->getSourceRange().getEnd().print(llvm::errs(), R.getSourceMgr());
    llvm::errs() << "\n";
  }

  // Override the method that gets called for each parsed top-level
  // declaration.
  bool HandleTopLevelDecl(DeclGroupRef DR) override {
    unsigned count = 0;
    SourceLocation lastSourceLoc;
    //llvm::errs() << "== Saw top-level decl\n";
    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
      // HACK: to get parent info, I have to do this, but I have no idea why.
      C.setTraversalScope({*b});
      // Traverse the declaration using our AST visitor.
      Visitor.TraverseDecl(*b);
      //(*b)->dump();
      ++count;
      lastSourceLoc = (*b)->getSourceRange().getEnd();
    }
    llvm::errs() << "== Processed  " << count << " top-level decl groups\n";
    llvm::errs() << "== The last one ended at  ";
    lastSourceLoc.print(llvm::errs(), R.getSourceMgr());
    llvm::errs() << " (written in main file? "
      << R.getSourceMgr().isWrittenInMainFile(lastSourceLoc)
      << ", presumed in main file? "
      << R.getSourceMgr().isInMainFile(lastSourceLoc)
      << "; immediate spelling loc: ";
    R.getSourceMgr().getImmediateSpellingLoc(lastSourceLoc).print(llvm::errs(), R.getSourceMgr());
    llvm::errs() << ")\n";
    // can we "project" the loc into the main file? i.e. where is it written in it?
    
    
    /* At some point, we reach the end of the file proper, and then start traversing
     * the template instantiations that are implied by various other stuff and which
     * clang has to elaborate in the AST for its own reasons. We have already
     * rewritten the source code, so if we go on processing them, we will double-
     * rewrite stuff which will be bad. How can we identify these instances? */
    
    return true;
  }

private:
  CCCPPPASTVisitor Visitor;
  Rewriter &R;
  ASTContext &C;
};

// For each source file provided to the tool, a new FrontendAction is created.
class MyFrontendAction : public ASTFrontendAction {
public:
  MyFrontendAction() {}
  void EndSourceFileAction() override {
    SourceManager &SM = TheRewriter.getSourceMgr();
    llvm::errs() << "== EndSourceFileAction for: "
                 << SM.getFileEntryForID(SM.getMainFileID())->getName() << "\n";

    // Now emit the rewritten buffer.
    TheRewriter.getEditBuffer(SM.getMainFileID()).write(llvm::outs());
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    llvm::errs() << "== Creating AST consumer for: " << file << "\n";
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<CCCPPPConsumer>(TheRewriter, CI.getASTContext());
  }

private:
  Rewriter TheRewriter;
};

/* Clang wants a "compilations database" and a "source path list".
 * We want to mimic the gcc command-line interface; since so (mostly)
 * does clang, we should be able to get what we want from libclang
 * code. How does it parse its argc and argv? Where is the clang
 * main(), even? It's in clang/tools/driver.
 * HACK: for now, don't mimic gcc's command line. Just use the
 * LLVM common options format, and let our wrapper script adapt.
 * (But see Attic/options.cpp for a partial attempt at the original.)
 */
static llvm::cl::OptionCategory CCCPPPCategory("CCCPPP");

int main(int argc, const char **argv) {
  /* How do we use an ordinary (gcc-like) compiler command line
   * to drive a clang tool?
   *
   * The CommonOptionsParser seems to want our command-line to
   * be structured as
   * <arguments in the cl syntax> -- <compiler options>.
   * Indeed if we want the tool to do anything, we need to pass '--' on
   * the compiler command line.
   *
   * For now we just rip off cilpp to create a new driver program that
   * exec's the tool (i.e. us) with a clang-tool-style command line.  */
  // build a CompilationsDatabase from argv
  //HackedOptionsParser op(argc, argv);
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, CCCPPPCategory);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());

  // ClangTool::run accepts a FrontendActionFactory, which is then used to
  // create new objects implementing the FrontendAction interface. Here we use
  // the helper newFrontendActionFactory to create a default factory that will
  // return a new MyFrontendAction object every time.
  // To further customize this, we could create our own factory class.
  return Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
}
