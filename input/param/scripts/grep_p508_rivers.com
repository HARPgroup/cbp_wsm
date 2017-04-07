#!/bin/csh

 cat sedtrn_header >river/nut/SEDTRN.csv
 grep '^S' /model/p508/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^P' /model/p58r/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^Y' /model/p58k/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^D' /model/p508/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^E' /model/p58r/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^R' /model/p58k/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^J' /model/p58d/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^W' /model/p58r/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^G' /model/p58r/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^X' /model/p58r/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^N' /model/p58d/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^T' /model/p58d/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^O' /model/p58d/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^M' /model/p58d/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^B' /model/p58d/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 grep '^K' /model/p58d/pp/param/river/withsed/SEDTRN.csv >>river/nut/SEDTRN.csv 
 echo end >> river/nut/SEDTRN.csv

