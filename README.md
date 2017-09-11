thank-you-stars
===============

[![Build Status](https://travis-ci.org/y-taka-23/thank-you-stars.svg?branch=master)](https://travis-ci.org/y-taka-23/thank-you-stars)

A tool for starring GitHub repositories. It detects dependent libraries
which are hosted on GitHub via `package.cabal` file,
and stars the repositories all at once.

Setup
-----

The project is managed by Stack, so you can install it simply:

```console
$ git clone https://github.com/y-taka-23/thank-you-stars.git
$ cd thank-you-stars
$ stack install
```

To star GitHub repositories, you have to get your personal access token.

1. Open https://github.com/settings/tokens and press "Generate new token."
1. Input the description and check only "public_repo" as a scope.
1. Save the token as `$HOME/.thank-you-stars.json`:

```json
{
    "token": "SET_YOUR_TOKEN_HERE"
}
```

Usage
-----

Run `thank-you-stars` in the root directory of your project.
Then it scans the `package.cabal` and the local Hackage DB,
stars your dependent libraries if they are hosted on GitHub.

```console
$ thank-you-stars
Starred! https://github.com/bos/aeson
Starred! https://github.com/bos/text
Starred! https://github.com/haskell/bytestring
Starred! https://github.com/haskell/cabal
Starred! https://github.com/haskell/containers
Starred! https://github.com/haskell/directory
Starred! https://github.com/haskell/filepath
Starred! https://github.com/mrkkrp/req
Starred! https://github.com/peti/hackage-db
```

License
-------

This project is released under the BSD 3-clause license.
For more details, see [LICENSE](./LICENSE) file.

Acknowledgement
---------------

This tool is greatly inspired by
[teppeis's JavaScript implementation](https://github.com/teppeis/thank-you-stars).
