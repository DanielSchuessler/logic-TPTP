## 0.4.5.0

* Fix compilation error with containers >=0.5.8 (Thanks to @msakai)

## 0.4.4.0

* Fix compilation error with transformers >=0.5.1 and GHC <7.10 (Thanks to @msakai)

## 0.4.3.0

* GHC 7.10.1 compatibility (Thanks to @agomezl)

## 0.4.2.0

* For transformers < 0.4.0.0, use the Eq/Ord/Show/Read Data.Functor.Identity orphan instances
  from transformers-compat instead of defining our own
