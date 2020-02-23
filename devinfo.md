A quick remark about maintaining this package.    

The illustrating example is contained in a private library, as Hackage doesn't yet support public libraries.    

To upload documentation on Hackage containing the example, first run:    

> cabal haddock acts --enable-documentation --haddock-for-hackage
> cabal haddock acts-examples --enable-documentation --haddock-for-hackage

Then manually transfer the files `Acts-Examples-{...}.html` and `src/Acts.Examples.{...}.html`
from `dist-newstyle/build/{arch}/{ghc-ver}/acts-{ver}/l/acts-examples/doc/html/acts-{ver}-docs` to `dist-newstyle/build/{arch}/{ghc-ver}/acts-{ver}/doc/html/acts-{ver}-docs`.

Create a gzipped tarball with `tar -czvf acts-{ver}.tar.gz acts-{ver}-docs`, and upload to hackage with `cabal upload -d acts-{ver}-docs.tar.gz --publish`.
