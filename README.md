[![Build Status](https://github.com/atgreen/daily-price-depot-droid/actions/workflows/build.yml/badge.svg)](https://github.com/atgreen/daily-price-depot-droid/actions)

# daily-price-depot-droid

* Add the following repository secrets to the github.com repo, under Setting > Secrets > New Repository Secret:
  * `REGISTRY_USERNAME`: your container registry username
  * `REGISTRY_PASSWORD`: your container registry password

* Create two container repositories at `quay.io/atgreen`:
  * `quay.io/atgreen/daily-price-depot-droid-base`: a base image to cache quicklisp contents and additional OS packages
  * `quay.io/atgreen/daily-price-depot-droid`: the final application
