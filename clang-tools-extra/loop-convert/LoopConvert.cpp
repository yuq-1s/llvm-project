#include <vector>
#include <unordered_map>
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
    : context_(Context), sm_(&context_->getSourceManager()) {}
  bool TraverseDecl(const Decl *D) {
    if (!context_) {
      llvm::outs() << "Context is NULL\n";
    } else {
      // if (D) {
      //   D->dump();
      // }
      // D->getLocation().dump(*sm_);
      relevant_ranges_.push_back(getFullRange(D));
    }
    RecursiveASTVisitor<FindRangeVisitor>::TraverseDecl(const_cast<Decl*>(D));
    return true;
  }

  bool TraverseStmt(Stmt* S) {
    assert(S);
    if (const auto *DRE = dyn_cast<DeclRefExpr>(S)) {
      // DRE->dump();
      // FIXME: LLVM ProgrammersManual says I should use InstVisitor instead?
      const auto* D = DRE->getDecl();
      if (const auto* FD = dyn_cast<FunctionDecl>(D)) {
        relevant_ranges_.push_back(getFullRange(FD->getDefinition()));
      } else if (const auto* VD = dyn_cast<VarDecl>(DRE->getDecl())) {
        relevant_ranges_.push_back(getFullRange(VD->getDefinition()));
      } else if (const auto* TD = dyn_cast<TagDecl>(DRE->getDecl())) {
        relevant_ranges_.push_back(getFullRange(TD->getDefinition()));
      } else {
        D->print(llvm::errs() << "[FATAL] No definitions found for ");
        return false;
      }
    }
    RecursiveASTVisitor<FindRangeVisitor>::TraverseStmt(S);
    return true;
  }

private:
  using VecPtrType = std::vector<CharSourceRange*>;
  const ASTContext *context_;
  const SourceManager *sm_;
  // FIXME: Can I use vector of references here ?
  std::vector<CharSourceRange> relevant_ranges_;

  auto mergeIntervals(VecPtrType&& ranges) {
    VecPtrType ret;
    std::sort(ranges.begin(), ranges.end(),
    [] (const auto& range1, const auto& range2) {
      return range1->getBegin() < range2->getBegin();
    });
    for (const auto& range : ranges) {
      if (ret.empty() || ret.back()->getEnd() < range->getBegin()) {
        ret.push_back(range);
      } else {
        ret.back()->setEnd(std::max(range->getEnd(), ret.back()->getEnd()));
      }
    }
    return ret;
  }

  auto getSourceToRangesMap() {
    std::unordered_map<std::string, VecPtrType> source_to_ranges;
    for (auto& range : relevant_ranges_) {
      assert(range.isValid());
      auto filename = sm_->getFilename(range.getBegin()).str();
      if (source_to_ranges.find(filename) != source_to_ranges.end()) {
        source_to_ranges[filename].push_back(&range);
      } else {
        source_to_ranges.emplace(filename, VecPtrType({&range}));
      }
    }
    return source_to_ranges;
  }

  auto reduce() {
    auto map = getSourceToRangesMap();
    VecPtrType ret;
    for (auto& item : map) {
      auto tmp = mergeIntervals(std::move(item.second));
      ret.insert(ret.end(), tmp.begin(), tmp.end());
    }
    return ret;
  }
  friend llvm::raw_fd_ostream& operator<<(llvm::raw_fd_ostream& os, FindRangeVisitor& visitor);
};


llvm::raw_fd_ostream& operator<<(llvm::raw_fd_ostream& os, FindRangeVisitor& visitor) {
  for (const auto& range : visitor.reduce()) {
    os << Lexer::getSourceText(*range, *visitor.sm_, LangOptions()).str();
  }
  visitor.relevant_ranges_.clear();
  return os;
}

class FunctionPrinter : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) override {
    auto D = Result.Nodes.getNodeAs<FunctionDecl>("function");
    const ASTContext* context = Result.Context;
    const auto* sm = &context->getSourceManager();
    llvm::outs() << "=========== " <<
        D->getNameAsString()
        // Lexer::getSourceText(sm->getExpansionRange(D->getBeginLoc()),
        //     *sm, LangOptions()).str()
        << " ============\n";
    FindRangeVisitor visitor(context);
    visitor.TraverseDecl(D);
    llvm::outs() << visitor;
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
