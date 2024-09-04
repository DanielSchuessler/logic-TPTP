## 0.5.1.0

* Fix compilation error with `mtl >=2.3`
* Fix an `-Woperator-whitespace-ext-conflict` warning
* Fix cabal warnings

## 0.5.0.0

* Add `GFormulaTerm` constructor to `GData` and support `$fot` `formula_data` (#1, #2, #19, thanks to @agomezl)
* Produce valid `$cnf` `formula_data` (#23)
* Improve test suites

## 0.4.7.0

* Fix to work with happy >=1.19.10

## 0.4.6.0

* Add Semigroup instances for Monoids and fix cabal-version warning (Thanks to @msakai)

## 0.4.5.0

* Fix compilation error with containers >=0.5.8 (Thanks to @msakai)

## 0.4.4.0

* Fix compilation error with transformers >=0.5.1 and GHC <7.10 (Thanks to @msakai)

## 0.4.3.0

* GHC 7.10.1 compatibility (Thanks to @agomezl)

## 0.4.2.0

* For transformers < 0.4.0.0, use the Eq/Ord/Show/Read Data.Functor.Identity orphan instances
  from transformers-compat instead of defining our own
