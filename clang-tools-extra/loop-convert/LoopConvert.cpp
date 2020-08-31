// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Lex/Lexer.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...\n");

// StatementMatcher LoopMatcher =
//   forStmt(hasLoopInit(declStmt(hasSingleDecl(varDecl(
//     hasInitializer(integerLiteral(equals(0)))))))).bind("forLoop");

// Expand to get the end location of the line where the EndLoc of the given
// Decl.
SourceLocation getLocForEndOfDecl(const Decl *D,
                                  const LangOptions &LangOpts = LangOptions()) {
  const auto &SM = D->getASTContext().getSourceManager();
  // If the expansion range is a character range, this is the location of
  // the first character past the end. Otherwise it's the location of the
  // first character in the final token in the range.
  auto EndExpansionLoc = SM.getExpansionRange(D->getEndLoc()).getEnd();
  std::pair<FileID, unsigned> LocInfo = SM.getDecomposedLoc(EndExpansionLoc);
  // Try to load the file buffer.
  bool InvalidTemp = false;
  llvm::StringRef File = SM.getBufferData(LocInfo.first, &InvalidTemp);
  if (InvalidTemp)
    return SourceLocation();

  const char *TokBegin = File.data() + LocInfo.second;
  // Lex from the start of the given location.
  Lexer Lex(SM.getLocForStartOfFile(LocInfo.first), LangOpts, File.begin(),
            TokBegin, File.end());

  llvm::SmallVector<char, 16> Line;
  // FIXME: this is a bit hacky to get ReadToEndOfLine work.
  Lex.setParsingPreprocessorDirective(true);
  Lex.ReadToEndOfLine(&Line);
  SourceLocation EndLoc = EndExpansionLoc.getLocWithOffset(Line.size());
  // If we already reach EOF, just return the EOF SourceLocation;
  // otherwise, move 1 offset ahead to include the trailing newline character
  // '\n'.
  return SM.getLocForEndOfFile(LocInfo.first) == EndLoc
             ? EndLoc
             : EndLoc.getLocWithOffset(1);
}

// Get full range of a Decl including the comments associated with it.
CharSourceRange getFullRange(const Decl *D,
                             const LangOptions &options = LangOptions()) {
  const auto &SM = D->getASTContext().getSourceManager();
  SourceRange Full(SM.getExpansionLoc(D->getBeginLoc()), getLocForEndOfDecl(D));
  // Expand to comments that are associated with the Decl.
  if (const auto *Comment = D->getASTContext().getRawCommentForDeclNoCache(D)) {
    if (SM.isBeforeInTranslationUnit(Full.getEnd(), Comment->getEndLoc()))
      Full.setEnd(Comment->getEndLoc());
    // FIXME: Don't delete a preceding comment, if there are no other entities
    // it could refer to.
    if (SM.isBeforeInTranslationUnit(Comment->getBeginLoc(), Full.getBegin()))
      Full.setBegin(Comment->getBeginLoc());
  }

  return CharSourceRange::getCharRange(Full);
}

class FindRangeVisitor : public RecursiveASTVisitor<FindRangeVisitor> {
public:
  explicit FindRangeVisitor(const ASTContext *Context)
    : context_(Context) {}
  bool TraverseDecl(const Decl *D) {
    const SourceManager *SM = &context_->getSourceManager();
    if (!context_) {
      llvm::outs() << "Context is NULL\n";
    } else {
      if (D) {
        D->dump();
      }
      D->getLocation().dump(*SM);
      auto source_text = Lexer::getSourceText(getFullRange(D), *SM, LangOptions()).str();
      llvm::outs() << source_text;
    }
    RecursiveASTVisitor<FindRangeVisitor>::TraverseDecl(const_cast<Decl*>(D));
    return true;
  }
private:
  const ASTContext* context_;
};

class FunctionPrinter : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) override {
    const ASTContext* context = Result.Context;
    FindRangeVisitor visitor(context);
    visitor.TraverseDecl(Result.Nodes.getNodeAs<FunctionDecl>("function"));
  }
};

int main(int argc, const char **argv) {
  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  FunctionPrinter funcPrinter;
  MatchFinder Finder;
  Finder.addMatcher(
    functionDecl().bind("function"),
    &funcPrinter);

  return Tool.run(newFrontendActionFactory(&Finder).get());
}
