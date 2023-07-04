{ pkgs, ... }: {
  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.compiler.ghc945;

  pre-commit.hooks.ormolu.enable = true;
}