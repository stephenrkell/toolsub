#include <sstream>
#include <string>
#ifdef USE_STD_UNIQUE_PTR
#include <memory>
#endif

#include "clang/AST/AST.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/ASTConsumer.h"
#ifdef HAVE_CLANG_AST_PARENTMAPCONTEXT_H
#include "clang/AST/ParentMapContext.h"
#endif
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
#include "llvm/ADT/Optional.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;
#ifdef HAVE_DYNTYPEDNODE_IN_CLANG_NAMESPACE
using clang::DynTypedNode;
#else
using clang::ast_type_traits::DynTypedNode;
#endif
#ifdef USE_STD_UNIQUE_PTR
using std::make_unique;
#else
using llvm::make_unique;
#endif
using llvm::Optional;

class SimpleRewriteASTVisitor : public RecursiveASTVisitor<SimpleRewriteASTVisitor> {
public:
  bool shouldTraversePostOrder() const /* override */ { return true; }
  SimpleRewriteASTVisitor(Rewriter &R, ASTContext &C) : TheRewriter(R), TheContext(C) {}

  /// helper that gets the original text for any expression (FIXME: can do more efficiently?)
  std::string GetReplacedText(Stmt *e)
  {
    /* CARE: nested rewritings!
     * If we replace some text, then replace some text that includes some replaced text,
     * we want to pick up the replaced version. The rewriter should be able to give us
     * this, probably. Instead of getting the character data from a memory buf, we
     * want to read from the rewrite rope.
     * FIXME: does this happen or not? */
    std::string s = TheRewriter.getRewrittenText(e->getSourceRange());
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
      << TheRewriter.getRewrittenText(e->getSourceRange()) << "'\n";
    // assert that it really has the effect we want
    assert(TheRewriter.getRewrittenText(e->getSourceRange()) == replacement);
  }
  unsigned SameFileDisplacement(const SourceLocation& start, const SourceLocation &end)
  {
    auto& sm = TheRewriter.getSourceMgr();
    std::pair<FileID, unsigned> locStart = sm.getDecomposedLoc(start);
    std::pair<FileID, unsigned> locEnd = sm.getDecomposedLoc(end);
    assert(locStart.first == locEnd.first);
    return locEnd.second - locStart.second;
  }
  void ReplaceBinaryExpressionInterstices(Expr *eOuter, Expr *eSubLeft, Expr *eSubRight,
    const std::string& leftInterstice,
    const std::string& midInterstice,
    const std::string& rightInterstice
  )
  {
    llvm::errs() << "Initially left-hand subexpression is:  `"
      << TheRewriter.getRewrittenText(eSubLeft->getSourceRange()) << "'\n";
    llvm::errs() << "Initially right-hand subexpression is:  `"
      << TheRewriter.getRewrittenText(eSubRight->getSourceRange()) << "'\n";
    llvm::errs() << "Initially whole binary expression is: `"
      << TheRewriter.getRewrittenText(eOuter->getSourceRange()) << "'\n";

#if 0
    SourceRange leftIntersticeRange(eOuter->getSourceRange().getBegin(),
       eSubLeft->getSourceRange().getBegin());
    SourceRange midIntersticeRange(eSubLeft->getSourceRange().getEnd(),
       eSubRight->getSourceRange().getBegin());
    SourceRange rightIntersticeRange(eSubRight->getSourceRange().getEnd(),
       eOuter->getSourceRange().getEnd());
#endif
    /* The above do not work. for 'p[0]' I get
Initially left interstice is:  `p'                 should be `'
Initially mid interstice is:  `p[0'                should be '['
Initially right interstice is: `0]'                should be ']'

       ... what is going on here? Could it be that 'end' is off by one?
       I think this does explain it, yes. "End" is interpreted as "last char".
       How can we make a legitimate right-open range from these?

       Since we need to ask the rewriter for a range, perhaps we should
       instead simply try to represent empty ranges separately.

       In general, it is necessary to think "block cursor" not "line cursor":
       the APIs really want to talk about characters, not positions between characters.

       And my theory about 'shifts' seems correct... the rewriter has a notion of "insert delta"
       https://clang.llvm.org/doxygen//Rewriter_8cpp_source.html#l00119

       // Add a delta so that future changes are offset correctly.
       AddInsertDelta(OrigOffset, Str.size());
     */
    auto rightOpenRange = [=](SourceLocation begin, SourceLocation end,
        bool beginIsAnEnd, bool endIsAnEnd) -> Optional<SourceRange> {
        //auto pair = sm.getDecomposedLoc(end);
        //return end.getLocWithOffset(1);
        SourceLocation beginLoc = beginIsAnEnd ? begin.getLocWithOffset(1) : begin;
        SourceLocation onePastEndLoc = endIsAnEnd ? end.getLocWithOffset(1) : end;
        if (beginLoc == onePastEndLoc) return Optional<SourceRange>();
        return SourceRange(beginLoc, onePastEndLoc.getLocWithOffset(-1));
    };

    Optional<SourceRange> leftIntersticeRange = rightOpenRange(eOuter->getSourceRange().getBegin(),
       eSubLeft->getSourceRange().getBegin(), false, false);
    Optional<SourceRange> midIntersticeRange = rightOpenRange(eSubLeft->getSourceRange().getEnd(),
       eSubRight->getSourceRange().getBegin(), true, false);
    Optional<SourceRange> rightIntersticeRange = rightOpenRange(eSubRight->getSourceRange().getEnd(),
       eOuter->getSourceRange().getEnd(), true, true);

    llvm::errs() << "Initially left interstice is:  `"
      << (!leftIntersticeRange ? "" : TheRewriter.getRewrittenText(*leftIntersticeRange)) << "'\n";
    llvm::errs() << "Initially mid interstice is:  `"
      << (!midIntersticeRange ? "" : TheRewriter.getRewrittenText(*midIntersticeRange)) << "'\n";
    llvm::errs() << "Initially right interstice is: `"
      << (!rightIntersticeRange ? "" : TheRewriter.getRewrittenText(*rightIntersticeRange)) << "'\n";

    /* Do three small rewrites, not one big one. */
    if (leftIntersticeRange) TheRewriter.ReplaceText(
      leftIntersticeRange->getBegin(),
      TheRewriter.getRewrittenText(*leftIntersticeRange).length(),
      leftInterstice
    ); 
    else { TheRewriter.InsertTextBefore(eOuter->getSourceRange().getBegin(), leftInterstice);
    /* FIXME: even though we do InsertTextBefore, the inserted text
     * still gets merged into the left-hand subexpression. What if we do InsertTextAfter
     * the previous character? */
    //else TheRewriter.InsertTextAfter(eOuter->getSourceRange().getBegin().getLocWithOffset(-1), leftInterstice);
    /* This 'works' but it breaks the "whole binary expression"! Since this now
     * does not include the left interstice.
     * We need to handle specially the case of replacing an empty interstice with a non-empty string,
     * to decouple the two expressions whose beginning or end coincide. 
     * XXX: how to do this?
     * Redux: if we do InsertTextBefore the full expr's first char,
     * it gets merged into both the left subexpression's and outer expression's text,
     * but we would like to to be in the latter but not the former.
     * If we to InsertTextAfter the preceding char,
     * it gets merged into neither expressions's text,
     * but we would like to to be in the latter but not the former.
     * I think the easiest way is to bump the left subexpression's start
     * forwards, XXX getSourceRange is const -- how do we update it?
     * Does it help if we instead try to mutate the binary expression node, since we know
     * more about its type?
     * */
     eSubLeft->getSourceRange().setBegin( // <-- this doesn't work because we are just updating a temporary
       eSubLeft->getSourceRange().getBegin().getLocWithOffset(leftInterstice.length())
     );
    }
    if (midIntersticeRange) TheRewriter.ReplaceText(
      midIntersticeRange->getBegin(),
      TheRewriter.getRewrittenText(*midIntersticeRange).length(),
      midInterstice
    ); else TheRewriter.InsertTextAfter(eSubLeft->getSourceRange().getEnd(), midInterstice);
    if (rightIntersticeRange) TheRewriter.ReplaceText(
      rightIntersticeRange->getBegin(),
      TheRewriter.getRewrittenText(*rightIntersticeRange).length(),
      rightInterstice
    ); else  TheRewriter.InsertTextAfter(eSubRight->getSourceRange().getEnd(), rightInterstice);

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
      std::string lhBefore = TheRewriter.getRewrittenText(e->getLHS()->getSourceRange());
      std::string rhBefore = TheRewriter.getRewrittenText(e->getRHS()->getSourceRange());
      // replace it with some text we have crafted
      QualType indexedType = e->getLHS()->getType();
      // is this definitely an array?
      std::string leftInterstice;
      std::string midInterstice = ", ";
      std::string rightInterstice = ")";
      if (!(indexedType.getTypePtr()->isTemplateTypeParmType()
        || indexedType.getTypePtr()->isDependentType()
        || indexedType.getTypePtr()->isInstantiationDependentType()
        || indexedType.getTypePtr()->isUndeducedType()))
      {
        leftInterstice = std::string("__primop_subscript<")
          + indexedType.getAsString()
          + " >()(";
        midInterstice = ", ";
        rightInterstice = ")";
      }
      else // nasty case
      {
        /* We want to print the type name, or an expression for it,
         * as it already appears in the code. But in many cases this
         * comes out as "<dependent type>", and I haven't figured out a
         * way to make clang print what we want. So use decltype() for now. */
        leftInterstice = std::string("__maybe_primop_subscript<")
          + "decltype("
          + lhBefore
          + "), !__has_subscript_overload<decltype(" + lhBefore + ")>::value>()(";
      }
      ReplaceBinaryExpressionInterstices(e, e->getLHS(), e->getRHS(),
          leftInterstice, midInterstice, rightInterstice);
      // after replacement, we should still have the same view of the subexpressions
      // FIXME: EXCEPT we can't do this if the LHS begins at the same place as the
      // outer expression
      //assert(e->getLHS()->getSourceRange().getBegin() == e->getSourceRange().getBegin()
      //|| lhBefore == TheRewriter.getRewrittenText(e->getLHS()->getSourceRange()));
      //assert(rhBefore == TheRewriter.getRewrittenText(e->getRHS()->getSourceRange()));
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
class SimpleRewriteConsumer : public ASTConsumer {
public:
  SimpleRewriteConsumer(Rewriter &R, ASTContext &C) : Visitor(R, C), R(R), C(C)
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
  SimpleRewriteASTVisitor Visitor;
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
    return make_unique<SimpleRewriteConsumer>(TheRewriter, CI.getASTContext());
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
static llvm::cl::OptionCategory SimpleRewriteCategory("SimpleRewrite");

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
#ifdef HAVE_COMMONOPTIONSPARSER_CREATE
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, SimpleRewriteCategory);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();
#else
  CommonOptionsParser OptionsParser(argc, argv, SimpleRewriteCategory);
#endif
  ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());

  // ClangTool::run accepts a FrontendActionFactory, which is then used to
  // create new objects implementing the FrontendAction interface. Here we use
  // the helper newFrontendActionFactory to create a default factory that will
  // return a new MyFrontendAction object every time.
  // To further customize this, we could create our own factory class.
  return Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
}
