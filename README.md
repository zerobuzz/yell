yell
====

Pass the command line you want to make loud as arguments to yell:

    yell ghc --make Main.hs

If you get any compiler errors, you will hopefully hear weird sounds.

If it doesn't work, try setting `YELL_VERBOSE` to `1` and try again;
if you get output like this:

    yell config:
    YellConfig
      { yellConfigRules =
          [ ( "error TS2095: Could not find symbol .*"
            , "185242__setuniman__abstract-0z78.ogg"
            )
            ....
          ]
      ....

    yell command:
    /home/rg/bin/ghc --make Main.hs

You are running yell.

For each command line tool, you can write your own config file with
your own rules.  The config file must have the name of the executable
with suffix `.rc`.

cabal install will move `./configs/*.rc` to a place where yell can
find it (usually under $HOME/.cabal), but you can also put your own
config files in `$HOME/.yell/`.

You can point symlinks to yell, and then call yell by the name of the
command you want to turn noisy.  If progName is not 'yell', it will be
used as the command and called again.  This is useful if you want to
call commands implicitly through scripts that you can't easily change.
(The mechanics for this are a little tricky.  See source code for
details.)
