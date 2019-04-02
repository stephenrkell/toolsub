#include <sstream>
#include <string>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
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

class CCCPPPASTVisitor : public RecursiveASTVisitor<CCCPPPASTVisitor> {
public:
  CCCPPPASTVisitor(Rewriter &R) : TheRewriter(R) {}

  bool VisitStmt(Stmt *s) {
    // Only care about If statements.
    if (isa<IfStmt>(s)) {
      IfStmt *IfStatement = cast<IfStmt>(s);
      Stmt *Then = IfStatement->getThen();

      //TheRewriter.InsertText(Then->getLocStart(), "// the 'if' part\n", true,
      //                       true);

      Stmt *Else = IfStatement->getElse();
      //if (Else)
      //  TheRewriter.InsertText(Else->getLocStart(), "// the 'else' part\n",
      //                         true, true);
    }

    return true;
  }

  bool VisitFunctionDecl(FunctionDecl *f) {
    // Only function definitions (with bodies), not declarations.
    if (f->hasBody()) {
      Stmt *FuncBody = f->getBody();

      // Type name as string
      QualType QT = f->getReturnType();
      std::string TypeStr = QT.getAsString();

      // Function name
      DeclarationName DeclName = f->getNameInfo().getName();
      std::string FuncName = DeclName.getAsString();

      // Add comment before
      std::stringstream SSBefore;
      SSBefore << "// Begin function " << FuncName << " returning " << TypeStr
               << "\n";
      SourceLocation ST = f->getSourceRange().getBegin();
      //TheRewriter.InsertText(ST, SSBefore.str(), true, true);

      // And after
      std::stringstream SSAfter;
      SSAfter << "\n// End function " << FuncName;
      ST = FuncBody->getLocEnd().getLocWithOffset(1);
      //TheRewriter.InsertText(ST, SSAfter.str(), true, true);
    }

    return true;
  }

private:
  Rewriter &TheRewriter;
};

// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser.
class CCCPPPConsumer : public ASTConsumer {
public:
  CCCPPPConsumer(Rewriter &R) : Visitor(R) {}

  // Override the method that gets called for each parsed top-level
  // declaration.
  bool HandleTopLevelDecl(DeclGroupRef DR) override {
    //llvm::errs() << "** Saw top-level decl\n";
    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
      // Traverse the declaration using our AST visitor.
      Visitor.TraverseDecl(*b);
      (*b)->dump();
    }
    return true;
  }

private:
  CCCPPPASTVisitor Visitor;
};

// For each source file provided to the tool, a new FrontendAction is created.
class MyFrontendAction : public ASTFrontendAction {
public:
  MyFrontendAction() {}
  void EndSourceFileAction() override {
    SourceManager &SM = TheRewriter.getSourceMgr();
    llvm::errs() << "** EndSourceFileAction for: "
                 << SM.getFileEntryForID(SM.getMainFileID())->getName() << "\n";

    // Now emit the rewritten buffer.
    TheRewriter.getEditBuffer(SM.getMainFileID()).write(llvm::outs());
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    llvm::errs() << "** Creating AST consumer for: " << file << "\n";
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<CCCPPPConsumer>(TheRewriter);
  }

private:
  Rewriter TheRewriter;
};

/* Clang wants a "compilations database" and a "source path list".
 * Since we mimic the gcc command-line interface and so (mostly)
 * does clang, we should be able to get what we want from libclang
 * code. How does it parse its argc and argv? Where is the clang
 * main(), even? It's in clang/tools/driver
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
  CommonOptionsParser OptionsParser(argc, argv, CCCPPPCategory);
  ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());

  // ClangTool::run accepts a FrontendActionFactory, which is then used to
  // create new objects implementing the FrontendAction interface. Here we use
  // the helper newFrontendActionFactory to create a default factory that will
  // return a new MyFrontendAction object every time.
  // To further customize this, we could create our own factory class.
  return Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
}
