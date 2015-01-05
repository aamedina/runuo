#!/bin/sh
dmcs -optimize+ -unsafe -t:library -out:Scripts/Output/RunUO.dll -nowarn:219,414 -d:MONO -recurse:Server/*.cs
