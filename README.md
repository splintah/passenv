# passenv

passenv is a tool and Haskell library for fetching secrets from a password store
managed by [`pass`](https://www.passwordstore.org/) and adding them to your
environment.

passenv is an embedded domain specific language (EDSL), which means that a
passenv configuration is is just a Haskell program that can make use of the
functions exported by passenv. You can therefore extend your passenv
configuration as you like in Haskell.

But: you don't need to know Haskell to write a simple passenv configuration. The
passenv EDSL *should* be abstract enough for someone to use it without knowing
Haskell.

## Example

Suppose you have a password file with the following files:

- `github.com` containing a [password](https://xkcd.com/936/):
   ```
   correct horse battery staple
   ```
- `mail` containing a password and an IMAP hostname:
   ```
   5t@mpc0ll3ct10n
   imap: mail.example.com
   ```
- `example.com` containing a password and a username:
   ```
   myverygoodpassword
   username: myusername
   ```

and we want to create an environment variable for the GitHub password, the mail
password, the IMAP hostname, and the username and password of `example.com` in
the format `username:password`.

The following program would do that:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Passenv

main :: IO ()
main = passenv def
  $ pass "github.com" (env
      [ ("GITHUB_PASSWORD", password)
      ])
 <> passKV "mail" (env
      [ ("MAIL_PASSWORD", password)
      , ("MAIL_IMAP_HOST", value "imap")
      ])
 <> passKV "example.com" (env
      [ ("EXAMPLE_AUTH", value "username" <> ":" <> password)
      ])
```

Then you can compile this program to `mypassenv` and run it:

```sh
$ mypassenv env
... # outer environment
GITHUB_PASSWORD=correct horse battery staple
MAIL_PASSWORD=5t@mpc0ll3ct10n
MAIL_IMAP_HOST=mail.example.com
EXAMPLE_AUTH=myusername:myverygoodpassword
```

This example runs the `env` command which prints the environment. As you can
see, passenv has added some variables to the environment.

### Explanation

Let's look at this example line by line.

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

This line enables the `OverloadedStrings` language extension. See the
documentation for `Passenv.Builder` for more information.
<!-- TODO: link to documentation. -->

```haskell
import Passenv
```

This line is quite self-explanatory: it `import`s the necessary functions from
`Passenv`.

```haskell
main :: IO ()
```

With this line, we declare that the function named `main` has type `IO :: ()`.
It doesn't really matters what this means, and you can even omit it if you like.

```haskell
main = passenv def
```

Here the definition of our program starts: it calls the `passenv` function with
the `def`ault options.

But the `passenv` function expects three arguments, the last of which is started
with a `$` (which makes it possible to omit the brackets around the third
argument).

```haskell
  $ pass "github.com" (env
      [ ("GITHUB_PASSWORD", password)
      ])
```

The `pass` function expects two arguments: the name of the file in the password
store (like you would supply it to the `pass` command) and a *builder*.

The name of the file in this case is `"github.com"`. We want to build an
environment, so we use the `env` builder. It is a function that expects a list
as its argument; this list contains pairs of builders for a variable name and
its value (both strings).

The `pass` function makes sure that we have available a `PasswordFile`, which is
the parsed representation of the file. Because a `PasswordFile` is an instance
of the type class `HasPassword` (if you don't know Haskell, just read: *because
a `PasswordFile` has a password*), we can use the `password` builder, which just
returns the password as a string.

The `env` builder will build a list with the new environment variables; in this
case, that list will be:

```haskell
[("GITHUB_PASSWORD", "correct horse battery staple")]`
```

The `passenv` function will run this environment builder, and execute the
process supplied as arguments to the program on the command line in the new
environment.

```haskell
 <> passKV "mail" (env
      [ ("MAIL_PASSWORD", password)
      , ("MAIL_IMAP_HOST", value "imap")
      ])
```

With the `<>` operator, we can append two builders. We can only append two
builders if their output is ‘appendable’. Or to put it in Haskell terms: a
builder is a `Monoid` if its output is a `Monoid`.

In this case, we append two environment builders, which means that the
environments will be appended.

Another function like `pass` is `passKV`. It doesn't parse the file as a
`PasswordFile`, but as a `KeyValueFile`. This file does not only have a
password, but it also `HasKeyValues`: the key-value pairs formatted as `key:
value` in the password file will be parsed into a `Map`, which is available to
the environment builder (and therefore tot the variable builders).

With the `value` builder, we can lookup a key-value pair by its key, as we do
with `"imap"`.

```haskell
 <> passKV "example.com" (
      [ ("EXAMPLE_AUTH", value "username" <> ":" <> password)
      ])
```

Finally, the `"example.com"` example shows how we can also append variable
builders. The value of the `EXAMPLE_AUTH` variable will be the username and the
password with a `:` in between.

Because we have enabled the `OverloadedStrings` language extension, we can also
use string literals as builders. (Otherwise we must write `value "username" <>
pure ":" <> password`).

---

For more examples, see the `app/` directory.

## Usage notes

It is recommended you configure `gpg-agent` to be stay authenticated for more
than zero seconds. See [`man gpg-agent`](https://linux.die.net/man/1/gpg-agent)
on how to do this; you can use the `default-cache-ttl` option.

## Motivation & inspiration

I manage most of my computer configuration using [home-manager], so my
configurations are written in [Nix]. I like to make my configuration available
on GitHub. But some of these configuration files contain personal secrets, which
I'd rather not share with everyone on the internet!

My solution was very easy: just don't commit files with secrets to GitHub. This
works great, but I'd still like to make these files available via GitHub,
because I want to synchronise them across multiple machines.

Because of this struggle, I decided to make passenv. The idea was inspired by
[vaultenv] and [xmonad] and [optparse-applicative] inspired me to write passenv
as an EDSL.

Because of this struggle, and because I experienced working with the nice
[vaultenv], being employed by [Channable], I decided to make passenv.

[home-manager]: https://github.com/rycee/home-manager
[Nix]: https://nixos.org
[vaultenv]: https://github.com/channable/vaultenv
[xmonad]: https://xmonad.org
[optparse-applicative]: https://github.com/pcapriotti/optparse-applicative

## License

Copyright (C) 2020 Splinter Suidman

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.
