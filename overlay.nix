{ inputs }:
final: prev:
let
  overlay = (hsSelf: hsSuper: {
    llvm-hs-pretty = 
    final.haskell.lib.compose.appendPatch ./patches/llvm-hs-pretty/rumkeller.patch
    hsSuper.llvm-hs-pretty;
    llvm-hs = 
    final.haskell.lib.compose.doJailbreak 
    (final.haskell.lib.compose.unmarkBroken 
    (final.haskell.lib.compose.appendPatch ./patches/jappeace-fix-build-llvm-hs.patch
    (final.haskell.lib.compose.appendPatch ./patches/travis-whitaker-llvm-hs.patch
    hsSuper.llvm-hs)));
  });
in {
  haskellPackages = prev.haskellPackages.extend overlay;

}
