#!/bin/sh
dmcs -optimize+ -unsafe -t:library -out:resources/clojure/1.7/RunUO.dll -nowarn:219,414 -d:MONO -recurse:resources/clojure/1.7/Server/*.cs
