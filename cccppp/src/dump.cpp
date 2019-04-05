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

// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser.
class CCCPPPConsumer : public ASTConsumer {
public:
  CCCPPPConsumer(ASTContext &C) : C(C) {}

  // Override the method that gets called for each parsed top-level
  // declaration.
  bool HandleTopLevelDecl(DeclGroupRef DR) override {
  	unsigned count = 0;
    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
      (*b)->dump();
      ++count;
    }
    llvm::errs() << "== Processed a group of " << count << " top-level decls\n";
    return true;
  }

private:
  ASTContext &C;
};

// For each source file provided to the tool, a new FrontendAction is created.
class MyFrontendAction : public ASTFrontendAction {
public:
  MyFrontendAction() {}
  void EndSourceFileAction() override {
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    return llvm::make_unique<CCCPPPConsumer>( CI.getASTContext());
  }
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
