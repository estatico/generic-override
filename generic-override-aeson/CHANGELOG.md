# Changelog for generic-override-aeson

## 0.4.0.0

* Add `WithAesonOptions` support
* Bumping dependency bounds to support generic-override 0.4
* Because this version of generic-override no longer uses `Overridden` under
    the hood, this solves a problem where `omitNothingFields` had no effect
    due to aeson's incoherent `Maybe` instance not being solved. The new
    encoding solves this problem and `omitNothingFields` works as expected.

## 0.3.0.0

* Bumping dependency bounds to support generic-override 0.3

## 0.0.0.2

* Bumping dependency bounds to support aeson 1.5

## 0.0.0.0

* Initial release
