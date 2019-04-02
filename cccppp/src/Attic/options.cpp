//===--- CommonOptionsParser.cpp - common options for clang tools ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/Tooling/Tooling.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Program.h"

#include "options.hpp"

using namespace clang::tooling;
using namespace llvm;

// void ArgumentsAdjustingCompilations::appendArgumentsAdjuster(
//     ArgumentsAdjuster Adjuster) {
//   Adjusters.push_back(std::move(Adjuster));
// }

// std::vector<CompileCommand> ArgumentsAdjustingCompilations::getCompileCommands(
//     StringRef FilePath) const {
//   return adjustCommands(Compilations->getCompileCommands(FilePath));
// }
// 
// std::vector<std::string>
// ArgumentsAdjustingCompilations::getAllFiles() const {
//   return Compilations->getAllFiles();
// }
// 
// std::vector<CompileCommand>
// ArgumentsAdjustingCompilations::getAllCompileCommands() const {
//   return adjustCommands(Compilations->getAllCompileCommands());
// }
// 
// std::vector<CompileCommand> ArgumentsAdjustingCompilations::adjustCommands(
//     std::vector<CompileCommand> Commands) const {
//   for (CompileCommand &Command : Commands)
//     for (const auto &Adjuster : Adjusters)
//       Command.CommandLine = Adjuster(Command.CommandLine, Command.Filename);
//   return Commands;
// }



  // FIXME: what is this "adjusting" business? was in "init" (which we inlined into
  // the constructor); commenting out for now
//   auto AdjustingCompilations =
//       llvm::make_unique<ArgumentsAdjustingCompilations>(
//           std::move(Compilations));
//   Adjuster =
//       getInsertArgumentAdjuster(ArgsBefore, ArgumentInsertPosition::BEGIN);
//   Adjuster = combineAdjusters(
//       std::move(Adjuster),
//       getInsertArgumentAdjuster(ArgsAfter, ArgumentInsertPosition::END));
//   AdjustingCompilations->appendArgumentsAdjuster(Adjuster);
//   Compilations = std::move(AdjustingCompilations);

// HACK: pasted from driver.cpp
std::string GetExecutablePath(const char *Argv0, bool CanonicalPrefixes) {
  if (!CanonicalPrefixes) {
    SmallString<128> ExecutablePath(Argv0);
    // Do a PATH lookup if Argv0 isn't a valid path.
    if (!llvm::sys::fs::exists(ExecutablePath))
      if (llvm::ErrorOr<std::string> P =
              llvm::sys::findProgramByName(ExecutablePath))
        ExecutablePath = *P;
    return ExecutablePath.str();
  }

  // This just needs to be some symbol in the binary; C++ doesn't
  // allow taking the address of ::main however.
  void *P = (void*) (intptr_t) GetExecutablePath;
  return llvm::sys::fs::getMainExecutable(Argv0, P);
}

HackedOptionsParser::HackedOptionsParser(
    int &argc, const char **argv, const char *Overview) {
  SmallVector<const char *, 256> argv_v;
  llvm::SpecificBumpPtrAllocator<char> ArgAllocator;
  std::error_code EC = llvm::sys::Process::GetArgumentVector(
      argv_v, llvm::makeArrayRef(argv, argc), ArgAllocator);
  if (EC) {
    llvm::errs() << "error: couldn't get arguments: " << EC.message() << '\n';
  }
  // we build an instance of the driver
  IntrusiveRefCntPtr<clang::DiagnosticOptions> DiagOpts = new clang::DiagnosticOptions();
  clang::TextDiagnosticPrinter *DiagClient
    = new clang::TextDiagnosticPrinter(llvm::errs(), &*DiagOpts);
  IntrusiveRefCntPtr<clang::DiagnosticIDs> DiagID(new clang::DiagnosticIDs());
  clang::DiagnosticsEngine Diags(DiagID, &*DiagOpts, DiagClient);
  std::string Path = GetExecutablePath(argv[0], true);
  clang::driver::Driver TheDriver(Path, llvm::sys::getDefaultTargetTriple(), Diags);
  std::unique_ptr<clang::driver::Compilation> C(TheDriver.BuildCompilation(argv_v));
  if (C && !C->containsError()) {
    // use C to build our own CompilationDatabase
    std::vector<std::string> args;
    for (unsigned i = 0; i < C->getInputArgs().getNumInputArgStrings(); ++i)
    {
      std::string s = C->getInputArgs().getArgString(i);
      llvm::errs() << "Saw an arg: " << s << "\n";
      /* GIANT HACK. If it doesn't begin with "-", add it. */
      if (s.length() > 0 && s.at(0) != '-') args.push_back(s);
    }
    Compilations = llvm::make_unique<FixedCompilationDatabase>(".", args /* vector of
     strings, one for every "non-positional" arg */);
    SourcePathList = args;
  }
  if (!Compilations) {
    llvm::report_fatal_error(
        "HackedOptionsParser: failed to parse command-line arguments.");
  }
}

