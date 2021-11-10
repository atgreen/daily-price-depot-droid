[![Build Status](https://github.com/atgreen/daily-price-depot-droid/actions/workflows/build.yml/badge.svg)](https://github.com/atgreen/daily-price-depot-droid/actions)

# daily-price-depot-droid

This software, which runs neatly as a pod on k8s, fetches historical
and daily price data from [Alpha Vantage](https://alphavantage.co) and
stores them in [ledger](https://ledger-cli.org)-compatible files in a
git repo (your "daily price depot"), ready to consume by `ledger`.

Sample /etc/daily-price-depot-droid/config.ini:

    ALPHAVANTAGE_API_KEY = "SFDGSO98W34K34LW"
    repo-git-uri = "https://username:password@git.example.com/username/daily-price-depot"
    equities = [ "IBM.NYSE", "KD.NYSE" ]
    fiats = [ "CAD", "EUR" ]
    commodities = [ "XAU", "XAG" ]
    cron-schedule = "10 16 * * *"
