{ pkgs, ... }: {
  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.compiler.ghc963;

  pre-commit.hooks.ormolu.enable = true;
}