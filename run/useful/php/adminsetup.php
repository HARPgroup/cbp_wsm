<?php
 
   # adminsetup
   # file contains definitions for database table formatting parameters
   # type - display type
   # params -
   #    #3 - select list - foreigntable:pkcol:listcols(csv):sortcol:showlabels
   #    #8 - scientific notation
   #    #9 - number format
   #    #10 - currency format
   #    #11 - 2 columns map - localvar1:localvar2:foreigntable:foreigncolumn1:foreigncolumn2:
   #                      listcolumns(csv):sortcol:showlabels(bool)
   #    #12 - formatted select - foreigntable:pkcol:listcols(csv):sortcol:showlabels
   #    #13 - 2 columns map, constant first paramter - constant:localvar2:foreigntable:
   #                      listcolumns(csv):sortcol:showlabels(bool)
   #    #15 - edit link
   #    #16 - subSelectList shows only a subset of the intems in the select list table,
            #   indicated by the seckeycol, the matching value of the seckeycol coming
            #   from the currentrecord, the value of the field named "myseckeycol" -
            #   params = foreigntable:pkcol:seckeycol:myseckeycol:listcols(csv):sortcol:showlabels
   #    #17 - mapped parameter value foreigntable:localkeycol:foreignkeycol:paramcol(csv)
   #    #18 - keyed map, has two local values to indicate the local entry in the mapping table
            # with one foreign key value, may be a multi-list
            # params ($maptable:$local1:$local2:$map1:$map2:$foreignmapcol:$foreigntable:$foreignkeycol:$paramcol:$ismulti:$numrows)
   $adminsetuparray = array(

      "wasteproduction"=>array(
          "table info"=>array("pk"=>"sourceid", "sortcol"=>"sourcename"),
          "column info"=>array(
              "sourceid"=>array("type"=>1,"params"=>"","label"=>"Source ID","visible"=>1),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1),
              "unitsperday"=>array("type"=>8,"params"=>"","label"=>"Units per day","visible"=>1)
           )
       ),
      "sourceloadtype"=>array(
          "table info"=>array("pk"=>"typeid", "sortcol"=>"sourcename"),
          "column info"=>array(
              "typeid"=>array("type"=>1,"params"=>"","label"=>"Type ID","visible"=>0),
              "sourceid"=>array("type"=>1,"params"=>"","label"=>"Source ID","visible"=>1),
              "sourceclass"=>array("type"=>3, "params"=>"sourceclass:classid:classname:classid:1", "label"=>"Source Class","visible"=>1),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1, "width"=>48),
              "auweight"=>array("type"=>1,"params"=>"","label"=>"AU Weight (default 1,000 lbs)","visible"=>1, "width"=>12),
              "avgweight"=>array("type"=>1,"params"=>"","label"=>"Avg. Wt.","visible"=>1, "width"=>12),
              "pollutantprod"=>array("type"=>1,"params"=>"","label"=>"Waste prod.","visible"=>1, "width"=>12),
              "produnits"=>array("type"=>1,"params"=>"","label"=>"Prod. units","visible"=>1, "width"=>24),
              "pollutantconc"=>array("type"=>1,"params"=>"","label"=>"Pollutant Concentration per Unit of Waste","visible"=>1, "width"=>16),
              "concunits"=>array("type"=>1,"params"=>"","label"=>"Pollutant Concentration Units","visible"=>1, "width"=>16),
              "storagedieoff"=>array("type"=>1,"params"=>"","label"=>"Factor for dieoff in storage (zeroth order per year)","visible"=>1, "width"=>8),
              "conv"=>array("type"=>1,"params"=>"","label"=>"Conversion Factor for Model Input","visible"=>1, "width"=>12),
              "convunits"=>array("type"=>1,"params"=>"","label"=>"Units for Conversion factor","visible"=>1, "width"=>16),
              "starttime"=>array("type"=>1,"params"=>"","label"=>"Starting time for waste production (hour 0-23)","visible"=>1, "width"=>16),
              "duration"=>array("type"=>1,"params"=>"","label"=>"Duration for waste production (hours)","visible"=>1, "width"=>16),
              "directfraction"=>array("type"=>1,"params"=>"","label"=>"Direct To Water Fraction (negative value indicates custom monthly distribution, set in source Distribution)","visible"=>1, "width"=>12),
              "comments"=>array("type"=>1,"params"=>"","label"=>"Comments","visible"=>1, "width"=>64),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0),
              "parentid"=>array("type"=>5, "params"=>"parentid:project:sourceloadtype:typeid:projectid:projectid:projectname:0:projectname", "label"=>"Parent ID","visible"=>1,"readonly"=>1),
              "inheritmode"=>array("type"=>3, "params"=>"inheritmode:inheritid:inheritmode:inheritmode:0", "label"=>"Inheritance Mode","visible"=>1),
              "copylink"=>array("type"=>15, "params"=>"edit_multisourcetypes.php:projectid:workspace:'Copy to Default Source Types':actiontype=copy", "label"=>"", "visible"=>1),
              "editlink"=>array("type"=>15, "params"=>"edit_multisourcetypes.php:projectid:workspace:'Edit':actiontype=edit", "label"=>"", "visible"=>1)
           )
       ),
      "sourcepollutants"=>array(
          "table info"=>array("pk"=>"typeid", "sortcol"=>"pollutantname"),
          "column info"=>array(
              "typeid"=>array("type"=>1,"params"=>"","label"=>"Type ID","visible"=>0),
              "sourcetypeid"=>array("type"=>1,"params"=>"","label"=>"Source ID","visible"=>0),
              "pollutantname"=>array("type"=>1,"params"=>"","label"=>"Pollutant Name","visible"=>1, "width"=>48),
              "pollutanttype"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:typeid:0", "label"=>"Pollutant Type","visible"=>1),
              "pollutantconc"=>array("type"=>1,"params"=>"","label"=>"Pollutant Concentration per Unit of Waste","visible"=>1, "width"=>16),
              "concunits"=>array("type"=>1,"params"=>"","label"=>"Pollutant Concentration Units","visible"=>1, "width"=>16),
              "storagedieoff"=>array("type"=>1,"params"=>"","label"=>"Factor for dieoff/sequestration in storage (zeroth order per year)","visible"=>1, "width"=>8),
              "volatilization"=>array("type"=>1,"params"=>"","label"=>"Volatilization rate during storage (zeroth order per year)","visible"=>1, "width"=>8),
              "conv"=>array("type"=>1,"params"=>"","label"=>"Conversion Factor for Model Input","visible"=>1, "width"=>12),
              "convunits"=>array("type"=>1,"params"=>"","label"=>"Units for Conversion factor","visible"=>1, "width"=>16),
              "comments"=>array("type"=>1,"params"=>"","label"=>"Comments","visible"=>1, "width"=>64),
              "parentid"=>array("type"=>5, "params"=>"project:sourceloadtype:typeid:projectid:typeid:projectname:0:projectname", "label"=>"parent ID","visible"=>1,"readonly"=>1),
              "inheritmode"=>array("type"=>3, "params"=>"inheritmode:inheritid:inheritmode:inheritmode:1", "label"=>"Inheritance Mode","visible"=>1),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0),
              "copylink"=>array("type"=>15, "params"=>"edit_multisourcetypes.php:projectid:workspace:'Copy to Default Source Types':actiontype=copy", "label"=>"", "visible"=>1),
              "editlink"=>array("type"=>15, "params"=>"edit_multisourcetypes.php:projectid:workspace:'Edit':actiontype=edit", "label"=>"", "visible"=>1)
           )
       ),
       "load_table"=>array(
          "table info"=>array("pk"=>"subshed", "sortcol"=>"subshed"),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Subshed","visible"=>1),
              "sourceid"=>array("type"=>1,"params"=>"","label"=>"Source ID","visible"=>1),
              "landuse"=>array("type"=>1, "params"=>"","label"=>"Landuse", "visible"=>1),
              "jan"=>array("type"=>8,"params"=>"0","label"=>"JAN","visible"=>1),
              "feb"=>array("type"=>8,"params"=>"0","label"=>"FEB","visible"=>1),
              "mar"=>array("type"=>8,"params"=>"0","label"=>"MAR","visible"=>1),
              "apr"=>array("type"=>8,"params"=>"0","label"=>"APR","visible"=>1),
              "may"=>array("type"=>8,"params"=>"0","label"=>"MAY","visible"=>1),
              "jun"=>array("type"=>8,"params"=>"0","label"=>"JUN","visible"=>1),
              "jul"=>array("type"=>8,"params"=>"0","label"=>"JUL","visible"=>1),
              "aug"=>array("type"=>8,"params"=>"0","label"=>"AUG","visible"=>1),
              "sep"=>array("type"=>8,"params"=>"0","label"=>"SEP","visible"=>1),
              "oct"=>array("type"=>8,"params"=>"0","label"=>"OCT","visible"=>1),
              "nov"=>array("type"=>8,"params"=>"0","label"=>"NOV","visible"=>1),
              "dec"=>array("type"=>8,"params"=>"0","label"=>"DEC","visible"=>1)
           )
       ),
      "multi_load_table"=>array(
          "table info"=>array("pk"=>"subshed", "sortcol"=>"subshed"),
          "column info"=>array(
              "perlndid"=>array("type"=>1,"params"=>"","label"=>"PERLND","visible"=>1),
              "sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0","label"=>"Source","visible"=>1),
              "jan"=>array("type"=>8,"params"=>"0","label"=>"JAN","visible"=>1),
              "feb"=>array("type"=>8,"params"=>"0","label"=>"FEB","visible"=>1),
              "mar"=>array("type"=>8,"params"=>"0","label"=>"MAR","visible"=>1),
              "apr"=>array("type"=>8,"params"=>"0","label"=>"APR","visible"=>1),
              "may"=>array("type"=>8,"params"=>"0","label"=>"MAY","visible"=>1),
              "jun"=>array("type"=>8,"params"=>"0","label"=>"JUN","visible"=>1),
              "jul"=>array("type"=>8,"params"=>"0","label"=>"JUL","visible"=>1),
              "aug"=>array("type"=>8,"params"=>"0","label"=>"AUG","visible"=>1),
              "sep"=>array("type"=>8,"params"=>"0","label"=>"SEP","visible"=>1),
              "oct"=>array("type"=>8,"params"=>"0","label"=>"OCT","visible"=>1),
              "nov"=>array("type"=>8,"params"=>"0","label"=>"NOV","visible"=>1),
              "dec"=>array("type"=>8,"params"=>"0","label"=>"DEC","visible"=>1)
           )
       ),
      "hspf_load_table"=>array(
          "table info"=>array("pk"=>"subshed", "sortcol"=>"subshed"),
          "column info"=>array(
              "perlndid"=>array("type"=>1,"params"=>"","label"=>"PERLND","visible"=>1),
              "jan"=>array("type"=>8,"params"=>"0","label"=>"JAN","visible"=>1),
              "feb"=>array("type"=>8,"params"=>"0","label"=>"FEB","visible"=>1),
              "mar"=>array("type"=>8,"params"=>"0","label"=>"MAR","visible"=>1),
              "apr"=>array("type"=>8,"params"=>"0","label"=>"APR","visible"=>1),
              "may"=>array("type"=>8,"params"=>"0","label"=>"MAY","visible"=>1),
              "jun"=>array("type"=>8,"params"=>"0","label"=>"JUN","visible"=>1),
              "jul"=>array("type"=>8,"params"=>"0","label"=>"JUL","visible"=>1),
              "aug"=>array("type"=>8,"params"=>"0","label"=>"AUG","visible"=>1),
              "sep"=>array("type"=>8,"params"=>"0","label"=>"SEP","visible"=>1),
              "oct"=>array("type"=>8,"params"=>"0","label"=>"OCT","visible"=>1),
              "nov"=>array("type"=>8,"params"=>"0","label"=>"NOV","visible"=>1),
              "dec"=>array("type"=>8,"params"=>"0","label"=>"DEC","visible"=>1)
           )
       ),
      "sources"=>array(
          "table info"=>array("pk"=>"sourceid", "sortcol"=>"subshed"),
          "column info"=>array(
              "sourceid"=>array("type"=>1,"params"=>"","label"=>"Source ID","visible"=>0),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Subshed ID","visible"=>0),
              "typeid"=>array("type"=>3, "params"=>"sourceloadtype:typeid:sourcename:typeid:0", "label"=>"Source Type","visible"=>1),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1),
              "distrotype"=>array("type"=>3, "params"=>"distrotype:distroid:distroname:distroname:0", "label"=>"Distribution Type","visible"=>1),
              "avgweight"=>array("type"=>1,"params"=>"","label"=>"Average Weight","visible"=>1),
              "editlink"=>array("type"=>15, "params"=>"editsource.php:projectid,sourcename:workspace:Edit", "label"=>"", "visible"=>1)
           )
       ),
      "monthlydistro"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"distroname"),
          "column info"=>array(
              "distroid"=>array("type"=>1,"params"=>"","label"=>"ID","visible"=>0),
              "sourceid"=>array("type"=>1,"params"=>"","label"=>"Source ID","visible"=>0),
              "spreadid"=>array("type"=>3,"params"=>"spreadtype:spreadid:spreadname:spreadname:0","label"=>"Spread Type","visible"=>1),
              "distroname"=>array("type"=>1,"params"=>"","label"=>"Distribution Name","visible"=>1, "width"=>48),
              "landuseid"=>array("type"=>16, "params"=>"landuses:luid:projectid:projectid:landuse:landuse:0", "label"=>"Destination Landuse", "visible"=>1),
              "jan"=>array("type"=>1,"params"=>"","label"=>"JAN","visible"=>1,"width"=>6),
              "feb"=>array("type"=>1,"params"=>"","label"=>"FEB","visible"=>1,"width"=>6),
              "mar"=>array("type"=>1,"params"=>"","label"=>"MAR","visible"=>1,"width"=>6),
              "apr"=>array("type"=>1,"params"=>"","label"=>"APR","visible"=>1,"width"=>6),
              "may"=>array("type"=>1,"params"=>"","label"=>"MAY","visible"=>1,"width"=>6),
              "jun"=>array("type"=>1,"params"=>"","label"=>"JUN","visible"=>1,"width"=>6),
              "jul"=>array("type"=>1,"params"=>"","label"=>"JUL","visible"=>1,"width"=>6),
              "aug"=>array("type"=>1,"params"=>"","label"=>"AUG","visible"=>1,"width"=>6),
              "sep"=>array("type"=>1,"params"=>"","label"=>"SEP","visible"=>1,"width"=>6),
              "oct"=>array("type"=>1,"params"=>"","label"=>"OCT","visible"=>1,"width"=>6),
              "nov"=>array("type"=>1,"params"=>"","label"=>"NOV","visible"=>1,"width"=>6),
              "dec"=>array("type"=>1,"params"=>"","label"=>"DEC","visible"=>1,"width"=>6)
           )
        ),
      "subluparams"=>array(
          "table info"=>array("pk"=>"projectid,paramtype,sublu", "sortcol"=>"subshed"),
          "column info"=>array(
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0),
              "deletelink"=>array("type"=>15, "params"=>"import_model_params.php:projectid:workspace:Delete:actiontype=delete", "label"=>"Delete", "visible"=>1),
              "thisyear"=>array("type"=>1,"params"=>"","label"=>"Year","visible"=>1),
              "sublu"=>array("type"=>1,"params"=>"","label"=>"SUBLU/PERLND","visible"=>1),
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub-shed ID","visible"=>1),
              "luname"=>array("type"=>1,"params"=>"","label"=>"Landuse Name","visible"=>1),
              "nmluname"=>array("type"=>1,"params"=>"","label"=>"NM Landuse Name","visible"=>1),
              "luarea"=>array("type"=>1,"params"=>"","label"=>"Landuse Area","visible"=>1),
              "paramtype"=>array("type"=>1,"params"=>"","label"=>"Parameter Type","visible"=>1),
              "ripbuffer"=>array("type"=>1,"params"=>"","label"=>"Model as riparian Buffer(Experimental)","visible"=>1),
              "forest"=>array("type"=>1,"params"=>"","label"=>"Forest Cover Factor","visible"=>1),
              "lzsn"=>array("type"=>1,"params"=>"","label"=>"Lower Zone Storage","visible"=>1),
              "infilt"=>array("type"=>1,"params"=>"","label"=>"Infiltration","visible"=>1),
              "lsur"=>array("type"=>1,"params"=>"","label"=>"Slope Length","visible"=>1),
              "slsur"=>array("type"=>1,"params"=>"","label"=>"Slope","visible"=>1),
              "kvary"=>array("type"=>1,"params"=>"","label"=>"KVARY","visible"=>1),
              "agwrc"=>array("type"=>1,"params"=>"","label"=>"AGWRC","visible"=>1),
              "petmax"=>array("type"=>1,"params"=>"","label"=>"PETMAX","visible"=>1),
              "petmin"=>array("type"=>1,"params"=>"","label"=>"PETMIN","visible"=>1),
              "infexp"=>array("type"=>1,"params"=>"","label"=>"INFEXP","visible"=>1),
              "infild"=>array("type"=>1,"params"=>"","label"=>"INFILD","visible"=>1),
              "deepfr"=>array("type"=>1,"params"=>"","label"=>"DEEPFR","visible"=>1),
              "basetp"=>array("type"=>1,"params"=>"","label"=>"BASETP","visible"=>1),
              "agwetp"=>array("type"=>1,"params"=>"","label"=>"AGWETP","visible"=>1),
              "cepsc"=>array("type"=>1,"params"=>"","label"=>"CEPSC","visible"=>1),
              "uzsn"=>array("type"=>1,"params"=>"","label"=>"UZSN","visible"=>1),
              "nsur"=>array("type"=>1,"params"=>"","label"=>"NSUR","visible"=>1),
              "intfw"=>array("type"=>1,"params"=>"","label"=>"INTFW","visible"=>1),
              "irc"=>array("type"=>1,"params"=>"","label"=>"IRC","visible"=>1),
              "lzetp"=>array("type"=>1,"params"=>"","label"=>"LZETP","visible"=>1),
              "ceps"=>array("type"=>1,"params"=>"","label"=>"CEPS","visible"=>1),
              "surs"=>array("type"=>1,"params"=>"","label"=>"SURS","visible"=>1),
              "uzs"=>array("type"=>1,"params"=>"","label"=>"UZS","visible"=>1),
              "ifws"=>array("type"=>1,"params"=>"","label"=>"IFWS","visible"=>1),
              "lzs"=>array("type"=>1,"params"=>"","label"=>"LZS","visible"=>1),
              "agws"=>array("type"=>1,"params"=>"","label"=>"AGWS","visible"=>1),
              "gwvs"=>array("type"=>1,"params"=>"","label"=>"GVWS","visible"=>1),
              "pct_nm"=>array("type"=>1,"params"=>"","label"=>"Percent of land under nutrient management","visible"=>1),
              "optn"=>array("type"=>1,"params"=>"","label"=>"Optimal Crop Nitrogen","visible"=>1),
              "optp"=>array("type"=>1,"params"=>"","label"=>"Optimal Crop Phosphorous","visible"=>1),
              "maxn"=>array("type"=>1,"params"=>"","label"=>"Maximum Crop Nitrogen Applied","visible"=>1),
              "maxp"=>array("type"=>1,"params"=>"","label"=>"Maximum Crop Phosphorous Applied","visible"=>1),
              "uptake_n"=>array("type"=>1,"params"=>"","label"=>"Max Uptake Nitrogen","visible"=>1),
              "uptake_p"=>array("type"=>1,"params"=>"","label"=>"Max Uptake Phosphorous","visible"=>1),
              "nrate"=>array("type"=>1,"params"=>"","label"=>"Nitrogen Application multiplier","visible"=>1),
              "prate"=>array("type"=>1,"params"=>"","label"=>"Phosphorus Application multiplier","visible"=>1),
              "legume_n"=>array("type"=>1,"params"=>"","label"=>"Amount Legume Nitrogen Credit","visible"=>1),
              "retsc"=>array("type"=>1,"params"=>"","label"=>"RETSC (Implnd)","visible"=>1)
           )
       ),
      "map_model_link"=>array(
          "table info"=>array("pk"=>"mapid", "sortcol"=>"isinput,reachid", "outputformat"=>"row"),
          "column info"=>array(
              "mapid"=>array("type"=>1,"params"=>"","label"=>"Map ID","visible"=>0),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0),
              "wdmid"=>array("type"=>1,"params"=>"","label"=>"WDM DSN #","visible"=>1),
              "dsnid"=>array("type"=>1,"params"=>"","label"=>"WDM DSN #","visible"=>0),
              "dsname"=>array("type"=>1,"params"=>"","label"=>"WDM DSN Name","visible"=>0),
              "reachid"=>array("type"=>1,"params"=>"","label"=>"Reach ID","visible"=>1),
              "filename"=>array("type"=>1,"params"=>"","label"=>"File Name","visible"=>1),
              "destname"=>array("type"=>1,"params"=>"","label"=>"Destination Reaches (csv, blank defaults to all)","visible"=>0),
              "dname2"=>array("type"=>1,"params"=>"","label"=>"Auxilliary destination","visible"=>0),
              "linktype"=>array("type"=>1,"params"=>"","label"=>"Link Type","visible"=>0),
              "seriestype"=>array("type"=>1,"params"=>"","label"=>"Series Type","visible"=>0),
              "numcols"=>array("type"=>1,"params"=>"","label"=>"Number of Inputs","visible"=>1),
              "seriestype"=>array("type"=>1,"params"=>"","label"=>"<br>Type of Series (1=mean,2=point)","visible"=>1),
              "timeint"=>array("type"=>1,"params"=>"","label"=>"<br>Output interval (in hours)","visible"=>0),
              "factor"=>array("type"=>1,"params"=>"","label"=>"Factor","visible"=>1),
              "isinput"=>array("type"=>1,"params"=>"","label"=>"Is an upstream input?","visible"=>0),
              "deletelink"=>array("type"=>15, "params"=>"edit_model_links.php:projectid:workspace:Delete:actiontype=delete", "label"=>"", "visible"=>1)
           )
        ),
      "landuses"=>array(
          "table info"=>array("pk"=>"luid", "sortcol"=>"landuse", "outputformat"=>"tablerow"),
          "column info"=>array(
              "luid"=>array("type"=>1,"params"=>"","label"=>"Key ID","visible"=>0),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0),
              "landuseid"=>array("type"=>1,"params"=>"","label"=>"LU Code","readonly"=>1,"visible"=>1,"width"=>3),
              "landusetype"=>array("type"=>3,"params"=>"lutype:typeid:typename:typename:0","label"=>"Secondary Type","visible"=>1),
              "major_lutype"=>array("type"=>3,"params"=>"major_lutype:lutype:lutypename:lutypename:0","label"=>"Major Type","visible"=>1),
              "eoflu"=>array("type"=>3,"params"=>"eof_landuses:eof_luid:eof_luname:eof_luname:0","label"=>"Edge of Field LU","visible"=>1),
              "bmps"=>array("type"=>18,"params"=>"map_landuse_bmp:projectid:hspflu:projectid:luname:bmpname:bmp_subtypes:bmpname:bmptext:1:5","label"=>"Eligible BMPs","visible"=>1),
              "landuse"=>array("type"=>1,"params"=>"","label"=>"Name","visible"=>1,"width"=>24),
              "pct_impervious"=>array("type"=>1,"params"=>"","label"=>"Imp. Frac.","visible"=>1,"width"=>3),
              "hspflu"=>array("type"=>1,"params"=>"","label"=>"Map to HSPF lu","readonly"=>1,"visible"=>1,"width"=>32),
              "deletelink"=>array("type"=>15, "params"=>"edit_landuses.php:projectid:workspace:Delete:actiontype=delete", "label"=>"", "visible"=>1)
           )
        ),
      "generic_model_link"=>array(
          "table info"=>array("pk"=>"mapid", "sortcol"=>"isinput,reachid", "outputformat"=>"row"),
          "column info"=>array(
              "mapid"=>array("type"=>1,"params"=>"","label"=>"Map ID","visible"=>0),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0),
              "isinput"=>array("type"=>3, "params"=>"tftable:tfvalue:tftext:tfvalue:0", "label"=>"Is Input?","visible"=>1),
              "wdmid"=>array("type"=>1,"params"=>"","label"=>"WDM #","visible"=>1),
              "dsnid"=>array("type"=>1,"params"=>"","label"=>"WDM DSN #","visible"=>1),
              "dsname"=>array("type"=>1,"params"=>"","label"=>"DSN Name","visible"=>1),
              "units"=>array("type"=>1,"params"=>"","label"=>"Units","visible"=>1),
              "interp"=>array("type"=>1,"params"=>"","label"=>"Interp. Type","visible"=>1),
              "factor"=>array("type"=>1,"params"=>"","label"=>"Factor","visible"=>1),
              "factext"=>array("type"=>1,"params"=>"","label"=>"Factor Text","visible"=>1),
              "destname"=>array("type"=>1,"params"=>"","label"=>"Target Name","visible"=>1),
              "reachid"=>array("type"=>1,"params"=>"","label"=>"Targ 1","visible"=>1),
              "reachid2"=>array("type"=>1,"params"=>"","label"=>"Targ 2","visible"=>1),
              "ingroup"=>array("type"=>1,"params"=>"","label"=>"In Group","visible"=>1),
              "inname"=>array("type"=>1,"params"=>"","label"=>"In Name","visible"=>1),
              "inum1"=>array("type"=>1,"params"=>"","label"=>"In Targ1","visible"=>1),
              "inum2"=>array("type"=>1,"params"=>"","label"=>"In Targ2","visible"=>1),
              "filename"=>array("type"=>1,"params"=>"","label"=>"File Name","visible"=>0),
              "linktype"=>array("type"=>1,"params"=>"","label"=>"Link Type","visible"=>0),
              "numcols"=>array("type"=>1,"params"=>"","label"=>"Number of Inputs","visible"=>0),
              "seriestype"=>array("type"=>1,"params"=>"","label"=>"<br>Type of Series (1=mean,2=point)","visible"=>0),
              "deletelink"=>array("type"=>15, "params"=>"edit_model_links.php:projectid:workspace:Delete:actiontype=delete", "label"=>"", "visible"=>1)
           )
        ),
      "hspfmonthly"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "distroid"=>array("type"=>1,"params"=>"","label"=>"ID","visible"=>0, "readonly"=>'1'),
              "distrodomain"=>array("type"=>1,"params"=>"","label"=>"Distribution Domain (landuse or subshed)","visible"=>0, "readonly"=>'0'),
              "paramname"=>array("type"=>1,"params"=>"","label"=>"Parameter Name","visible"=>1, "width"=>12, "readonly"=>'1'),
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub-watershed ID","visible"=>1, "width"=>4, "readonly"=>'0'),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0, "width"=>4, "readonly"=>'0'),
              "landuseid"=>array("type"=>1,"params"=>"","label"=>"Landuse ID","visible"=>1, "width"=>4, "readonly"=>'0'),
              "hspf_lu"=>array("type"=>1,"params"=>"","label"=>"Landuse","visible"=>1, "width"=>4, "readonly"=>'0'),
              "distroname"=>array("type"=>1,"params"=>"","label"=>"Distribution Name","visible"=>1, "width"=>48, "readonly"=>'1'),
              "jan"=>array("type"=>1,"params"=>"","label"=>"JAN","visible"=>1,"width"=>6),
              "feb"=>array("type"=>1,"params"=>"","label"=>"FEB","visible"=>1,"width"=>6),
              "mar"=>array("type"=>1,"params"=>"","label"=>"MAR","visible"=>1,"width"=>6),
              "apr"=>array("type"=>1,"params"=>"","label"=>"APR","visible"=>1,"width"=>6),
              "may"=>array("type"=>1,"params"=>"","label"=>"MAY","visible"=>1,"width"=>6),
              "jun"=>array("type"=>1,"params"=>"","label"=>"JUN","visible"=>1,"width"=>6),
              "jul"=>array("type"=>1,"params"=>"","label"=>"JUL","visible"=>1,"width"=>6),
              "aug"=>array("type"=>1,"params"=>"","label"=>"AUG","visible"=>1,"width"=>6),
              "sep"=>array("type"=>1,"params"=>"","label"=>"SEP","visible"=>1,"width"=>6),
              "oct"=>array("type"=>1,"params"=>"","label"=>"OCT","visible"=>1,"width"=>6),
              "nov"=>array("type"=>1,"params"=>"","label"=>"NOV","visible"=>1,"width"=>6),
              "dec"=>array("type"=>1,"params"=>"","label"=>"DEC","visible"=>1,"width"=>6)
           )
        ),
      "adjmonthly"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "jan"=>array("type"=>1,"params"=>"","label"=>"JAN","visible"=>1,"width"=>6),
              "feb"=>array("type"=>1,"params"=>"","label"=>"FEB","visible"=>1,"width"=>6),
              "mar"=>array("type"=>1,"params"=>"","label"=>"MAR","visible"=>1,"width"=>6),
              "apr"=>array("type"=>1,"params"=>"","label"=>"APR","visible"=>1,"width"=>6),
              "may"=>array("type"=>1,"params"=>"","label"=>"MAY","visible"=>1,"width"=>6),
              "jun"=>array("type"=>1,"params"=>"","label"=>"JUN","visible"=>1,"width"=>6),
              "jul"=>array("type"=>1,"params"=>"","label"=>"JUL","visible"=>1,"width"=>6),
              "aug"=>array("type"=>1,"params"=>"","label"=>"AUG","visible"=>1,"width"=>6),
              "sep"=>array("type"=>1,"params"=>"","label"=>"SEP","visible"=>1,"width"=>6),
              "oct"=>array("type"=>1,"params"=>"","label"=>"OCT","visible"=>1,"width"=>6),
              "nov"=>array("type"=>1,"params"=>"","label"=>"NOV","visible"=>1,"width"=>6),
              "dec"=>array("type"=>1,"params"=>"","label"=>"DEC","visible"=>1,"width"=>6)
           )
        ),
      "subwaterbysource"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub ID","visible"=>1, "readonly"=>'1'),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutantname"=>array("type"=>1,"params"=>"","label"=>"Pollutant Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutanttype"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:pollutantname:0", "label"=>"Pollutant Type","visible"=>1),
              "avgweight"=>array("type"=>1,"params"=>"","label"=>"Avg. Wt.","visible"=>1, "width"=>12, "readonly"=>'1'),
              "auweight"=>array("type"=>1,"params"=>"","label"=>"AU Wt.","visible"=>1, "width"=>12, "readonly"=>'1'),
              "aucount"=>array("type"=>9,"params"=>"1","label"=>"Animal Units","visible"=>1, "width"=>12, "readonly"=>'1'),
              "pollutantprod"=>array("type"=>9,"params"=>"2", "label"=>"(lb/au/day)", "visible"=>1, "width"=>12, "readonly"=>'1'),
              "pollutantconc"=>array("type"=>9,"params"=>"6", "label"=>"concentration / lb", "visible"=>1, "width"=>12, "readonly"=>'1'),
              "actualpop"=>array("type"=>9,"params"=>"0","label"=>"Pop.","visible"=>1, "width"=>12, "readonly"=>'1'),
              "annualproduction"=>array("type"=>1,"params"=>"0","label"=>"Annual Production","visible"=>9, "width"=>12, "readonly"=>'1'),
              "conv"=>array("type"=>1,"params"=>"","label"=>"Conversion Factor","visible"=>1, "width"=>12, "readonly"=>'1'),
              "convunits"=>array("type"=>1,"params"=>"","label"=>"Conv. Units","visible"=>1, "width"=>12, "readonly"=>'1'),
              "sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0" , "label"=>"Source Name","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "starttime"=>array("type"=>1,"params"=>"","label"=>"Starting time for waste production (hour 0-23)","visible"=>1, "width"=>16),
              "duration"=>array("type"=>1,"params"=>"","label"=>"Duration for waste production (hours)","visible"=>1, "width"=>16),
              "sourceclass"=>array("type"=>3, "params"=>"sourceclass:classid:classname:classid:1", "label"=>"Source Class","visible"=>1),
              "jan"=>array("type"=>8,"params"=>"0","label"=>"JAN","visible"=>1),
              "feb"=>array("type"=>8,"params"=>"0","label"=>"FEB","visible"=>1),
              "mar"=>array("type"=>8,"params"=>"0","label"=>"MAR","visible"=>1),
              "apr"=>array("type"=>8,"params"=>"0","label"=>"APR","visible"=>1),
              "may"=>array("type"=>8,"params"=>"0","label"=>"MAY","visible"=>1),
              "jun"=>array("type"=>8,"params"=>"0","label"=>"JUN","visible"=>1),
              "jul"=>array("type"=>8,"params"=>"0","label"=>"JUL","visible"=>1),
              "aug"=>array("type"=>8,"params"=>"0","label"=>"AUG","visible"=>1),
              "sep"=>array("type"=>8,"params"=>"0","label"=>"SEP","visible"=>1),
              "oct"=>array("type"=>8,"params"=>"0","label"=>"OCT","visible"=>1),
              "nov"=>array("type"=>8,"params"=>"0","label"=>"NOV","visible"=>1),
              "dec"=>array("type"=>8,"params"=>"0","label"=>"DEC","visible"=>1)
           )
        ),
      "subsource"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub ID","visible"=>1, "readonly"=>'1'),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutantname"=>array("type"=>1,"params"=>"","label"=>"Pollutant Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutanttype"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:pollutantname:0", "label"=>"Pollutant Type","visible"=>1),
              "produnits"=>array("type"=>1,"params"=>"","label"=>"Source  Production Units","visible"=>1, "width"=>16),
              "concunits"=>array("type"=>1,"params"=>"","label"=>"Pollutant Concentration Units","visible"=>1, "width"=>16),
              "avgweight"=>array("type"=>1,"params"=>"","label"=>"Avg. Wt.","visible"=>1, "width"=>12, "readonly"=>'1'),
              "auweight"=>array("type"=>1,"params"=>"","label"=>"AU Wt.","visible"=>1, "width"=>12, "readonly"=>'1'),
              "aucount"=>array("type"=>1,"params"=>"","label"=>"Units","visible"=>1, "width"=>12, "readonly"=>'1'),
              "pollutantprod"=>array("type"=>1,"params"=>"","label"=>"(lb/au/day)","visible"=>1, "width"=>12, "readonly"=>'1'),
              "actualpop"=>array("type"=>1,"params"=>"","label"=>"Pop.","visible"=>1, "width"=>12, "readonly"=>'1'),
              "sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0", "label"=>"Source Name","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "starttime"=>array("type"=>1,"params"=>"","label"=>"Starting time for waste production (hour 0-23)","visible"=>1, "width"=>16),
              "duration"=>array("type"=>1,"params"=>"","label"=>"Duration for waste production (hours)","visible"=>1, "width"=>16),
              "sourceclass"=>array("type"=>3, "params"=>"sourceclass:classid:classname:classid:1", "label"=>"Source Class","visible"=>1),
              "jan"=>array("type"=>9,"params"=>"0","label"=>"JAN","visible"=>1),
              "feb"=>array("type"=>9,"params"=>"0","label"=>"FEB","visible"=>1),
              "mar"=>array("type"=>9,"params"=>"0","label"=>"MAR","visible"=>1),
              "apr"=>array("type"=>9,"params"=>"0","label"=>"APR","visible"=>1),
              "may"=>array("type"=>9,"params"=>"0","label"=>"MAY","visible"=>1),
              "jun"=>array("type"=>9,"params"=>"0","label"=>"JUN","visible"=>1),
              "jul"=>array("type"=>9,"params"=>"0","label"=>"JUL","visible"=>1),
              "aug"=>array("type"=>9,"params"=>"0","label"=>"AUG","visible"=>1),
              "sep"=>array("type"=>9,"params"=>"0","label"=>"SEP","visible"=>1),
              "oct"=>array("type"=>9,"params"=>"0","label"=>"OCT","visible"=>1),
              "nov"=>array("type"=>9,"params"=>"0","label"=>"NOV","visible"=>1),
              "dec"=>array("type"=>9,"params"=>"0","label"=>"DEC","visible"=>1)
           )
        ),
      "lusource"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"subshedid", "outputformat"=>"row"),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub ID","visible"=>1, "readonly"=>'1'),
              "landuseid"=>array("type"=>3, "params"=>"landuses:luid:landuse:landuse:0","label"=>"Landuse","visible"=>1, "readonly"=>'0', "width"=>24),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutantname"=>array("type"=>1,"params"=>"","label"=>"Pollutant Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualstored"=>array("type"=>9,"params"=>"0","label"=>"Stored","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualvolatilized"=>array("type"=>9,"params"=>"0","label"=>"Volatilized","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualapplied"=>array("type"=>9,"params"=>"0","label"=>"Applied","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutanttype"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:pollutantname:0", "label"=>"Pollutant Type","visible"=>1),"sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0", "label"=>"Source Name","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "jan"=>array("type"=>9,"params"=>"2","label"=>"JAN","visible"=>1),
              "feb"=>array("type"=>9,"params"=>"2","label"=>"FEB","visible"=>1),
              "mar"=>array("type"=>9,"params"=>"2","label"=>"MAR","visible"=>1),
              "apr"=>array("type"=>9,"params"=>"2","label"=>"APR","visible"=>1),
              "may"=>array("type"=>9,"params"=>"2","label"=>"MAY","visible"=>1),
              "jun"=>array("type"=>9,"params"=>"2","label"=>"JUN","visible"=>1),
              "jul"=>array("type"=>9,"params"=>"2","label"=>"JUL","visible"=>1),
              "aug"=>array("type"=>9,"params"=>"2","label"=>"AUG","visible"=>1),
              "sep"=>array("type"=>9,"params"=>"2","label"=>"SEP","visible"=>1),
              "oct"=>array("type"=>9,"params"=>"2","label"=>"OCT","visible"=>1),
              "nov"=>array("type"=>9,"params"=>"2","label"=>"NOV","visible"=>1),
              "dec"=>array("type"=>9,"params"=>"2","label"=>"DEC","visible"=>1)
           )
        ),
      "monperunitarea"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"subshedid", "outputformat"=>"row"),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub ID","visible"=>1, "readonly"=>'1'),
              "landuseid"=>array("type"=>3, "params"=>"landuses:luid:landuse:landuse:0","label"=>"Landuse","visible"=>1, "readonly"=>'0', "width"=>24),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutantname"=>array("type"=>1,"params"=>"","label"=>"Pollutant Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualstored"=>array("type"=>9,"params"=>"0","label"=>"Stored","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualvolatilized"=>array("type"=>9,"params"=>"0","label"=>"Volatilized","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualapplied"=>array("type"=>9,"params"=>"2","label"=>"Applied","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutanttype"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:pollutantname:0", "label"=>"Pollutant Type","visible"=>1),
              "sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0", "label"=>"Source Name","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "jan"=>array("type"=>9,"params"=>"6","label"=>"JAN lb/ac/day","visible"=>1),
              "feb"=>array("type"=>9,"params"=>"6","label"=>"FEB lb/ac/day","visible"=>1),
              "mar"=>array("type"=>9,"params"=>"6","label"=>"MAR lb/ac/day","visible"=>1),
              "apr"=>array("type"=>9,"params"=>"6","label"=>"APR lb/ac/day","visible"=>1),
              "may"=>array("type"=>9,"params"=>"6","label"=>"MAY lb/ac/day","visible"=>1),
              "jun"=>array("type"=>9,"params"=>"6","label"=>"JUN lb/ac/day","visible"=>1),
              "jul"=>array("type"=>9,"params"=>"6","label"=>"JUL lb/ac/day","visible"=>1),
              "aug"=>array("type"=>9,"params"=>"6","label"=>"AUG lb/ac/day","visible"=>1),
              "sep"=>array("type"=>9,"params"=>"6","label"=>"SEP lb/ac/day","visible"=>1),
              "oct"=>array("type"=>9,"params"=>"6","label"=>"OCT lb/ac/day","visible"=>1),
              "nov"=>array("type"=>9,"params"=>"6","label"=>"NOV lb/ac/day","visible"=>1),
              "dec"=>array("type"=>9,"params"=>"6","label"=>"DEC lb/ac/day","visible"=>1)
           )
        ),
      "yearperunitarea"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"subshedid", "outputformat"=>"row"),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub ID","visible"=>1, "readonly"=>'1'),
              "sublu"=>array("type"=>1,"params"=>"","label"=>"Sublu","visible"=>1, "readonly"=>'0'),
              "showlu"=>array("type"=>1, "params"=>"landuses:luid:landuse:landuse:0","label"=>"Landuse","visible"=>1, "readonly"=>'0', "width"=>24),
              "landuseid"=>array("type"=>3, "params"=>"landuses:luid:landuse:landuse:0","label"=>"Landuse","visible"=>1, "readonly"=>'0', "width"=>24),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutantname"=>array("type"=>1,"params"=>"","label"=>"Pollutant Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualstored"=>array("type"=>9,"params"=>"0","label"=>"Stored","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualvolatilized"=>array("type"=>9,"params"=>"0","label"=>"Volatilized","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualapplied"=>array("type"=>9,"params"=>"2","label"=>"Applied lb/ac/yr","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutanttype"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:pollutantname:0", "label"=>"Pollutant Type","visible"=>1),
              "luarea"=>array("type"=>9,"params"=>"2","label"=>"Area of Land","visible"=>1, "readonly"=>'0', "width"=>24),
              "maxn"=>array("type"=>9,"params"=>"2","label"=>"N capacity lb/ac/yr","visible"=>1, "readonly"=>'0', "width"=>24),
              "maxp"=>array("type"=>9,"params"=>"2","label"=>"P capacity lb/ac/yr","visible"=>1, "readonly"=>'0', "width"=>24),
              "pcap"=>array("type"=>9,"params"=>"2","label"=>"P capacity lb/yr","visible"=>1, "readonly"=>'0', "width"=>24),
              "ncap"=>array("type"=>9,"params"=>"2","label"=>"N capacity lb/yr","visible"=>1, "readonly"=>'0', "width"=>24),
              "sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0", "label"=>"Source Name","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "jan"=>array("type"=>9,"params"=>"6","label"=>"JAN lb/ac/mo","visible"=>1),
              "feb"=>array("type"=>9,"params"=>"6","label"=>"FEB lb/ac/mo","visible"=>1),
              "mar"=>array("type"=>9,"params"=>"6","label"=>"MAR lb/ac/mo","visible"=>1),
              "apr"=>array("type"=>9,"params"=>"6","label"=>"APR lb/ac/mo","visible"=>1),
              "may"=>array("type"=>9,"params"=>"6","label"=>"MAY lb/ac/mo","visible"=>1),
              "jun"=>array("type"=>9,"params"=>"6","label"=>"JUN lb/ac/mo","visible"=>1),
              "jul"=>array("type"=>9,"params"=>"6","label"=>"JUL lb/ac/mo","visible"=>1),
              "aug"=>array("type"=>9,"params"=>"6","label"=>"AUG lb/ac/mo","visible"=>1),
              "sep"=>array("type"=>9,"params"=>"6","label"=>"SEP lb/ac/mo","visible"=>1),
              "oct"=>array("type"=>9,"params"=>"6","label"=>"OCT lb/ac/mo","visible"=>1),
              "nov"=>array("type"=>9,"params"=>"6","label"=>"NOV lb/ac/mo","visible"=>1),
              "dec"=>array("type"=>9,"params"=>"6","label"=>"DEC lb/ac/mo","visible"=>1)
           )
        ),
      "sumbydistro"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub ID","visible"=>1, "readonly"=>'1'),
              "landuseid"=>array("type"=>3, "params"=>"landuses:luid:landuse:landuse:0","label"=>"Landuse","visible"=>1, "readonly"=>'0', "width"=>24),
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutantname"=>array("type"=>1,"params"=>"","label"=>"Pollutant Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutanttype"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:pollutantname:0", "label"=>"Pollutant Type","visible"=>1),
              "spreadid"=>array("type"=>3,"params"=>"spreadtype:spreadid:spreadname:spreadname:0","label"=>"Distribution Type","visible"=>1, "width"=>32, "readonly"=>'1'),
              "annualapplied"=>array("type"=>9,"params"=>"6","label"=>"Annual Total","visible"=>9, "width"=>12, "readonly"=>'1'),
              "jan"=>array("type"=>9,"params"=>"6","label"=>"JAN lb/ac/mo","visible"=>1),
              "feb"=>array("type"=>9,"params"=>"6","label"=>"FEB lb/ac/mo","visible"=>1),
              "mar"=>array("type"=>9,"params"=>"6","label"=>"MAR lb/ac/mo","visible"=>1),
              "apr"=>array("type"=>9,"params"=>"6","label"=>"APR lb/ac/mo","visible"=>1),
              "may"=>array("type"=>9,"params"=>"6","label"=>"MAY lb/ac/mo","visible"=>1),
              "jun"=>array("type"=>9,"params"=>"6","label"=>"JUN lb/ac/mo","visible"=>1),
              "jul"=>array("type"=>9,"params"=>"6","label"=>"JUL lb/ac/mo","visible"=>1),
              "aug"=>array("type"=>9,"params"=>"6","label"=>"AUG lb/ac/mo","visible"=>1),
              "sep"=>array("type"=>9,"params"=>"6","label"=>"SEP lb/ac/mo","visible"=>1),
              "oct"=>array("type"=>9,"params"=>"6","label"=>"OCT lb/ac/mo","visible"=>1),
              "nov"=>array("type"=>9,"params"=>"6","label"=>"NOV lb/ac/mo","visible"=>1),
              "dec"=>array("type"=>9,"params"=>"6","label"=>"DEC lb/ac/mo","visible"=>1)
           )
        ),
      "sumbysourcedistro"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub ID","visible"=>1, "readonly"=>'1'),
              "landuseid"=>array("type"=>3, "params"=>"landuses:luid:landuse:landuse:0","label"=>"Landuse","visible"=>1, "readonly"=>'0', "width"=>24),
              "sourceid"=>array("type"=>3, "params"=>"sources:sourceid:sourcename:sourcename:0", "label"=>"Source","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutantname"=>array("type"=>1,"params"=>"","label"=>"Pollutant Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutanttype"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:pollutantname:0", "label"=>"Pollutant Type","visible"=>1),
              "spreadid"=>array("type"=>3,"params"=>"spreadtype:spreadid:spreadname:spreadname:0","label"=>"Distribution Type","visible"=>1, "width"=>32, "readonly"=>'1'),
              "annualapplied"=>array("type"=>9,"params"=>"6","label"=>"Annual Total","visible"=>9, "width"=>12, "readonly"=>'1'),
              "maxn"=>array("type"=>9,"params"=>"2","label"=>"Nitrogen App Rate","visible"=>1),
              "maxp"=>array("type"=>9,"params"=>"2","label"=>"Phosphorus App Rate","visible"=>1),
              "jan"=>array("type"=>9,"params"=>"2","label"=>"JAN lb/ac/mo","visible"=>1),
              "feb"=>array("type"=>9,"params"=>"2","label"=>"FEB lb/ac/mo","visible"=>1),
              "mar"=>array("type"=>9,"params"=>"2","label"=>"MAR lb/ac/mo","visible"=>1),
              "apr"=>array("type"=>9,"params"=>"2","label"=>"APR lb/ac/mo","visible"=>1),
              "may"=>array("type"=>9,"params"=>"2","label"=>"MAY lb/ac/mo","visible"=>1),
              "jun"=>array("type"=>9,"params"=>"2","label"=>"JUN lb/ac/mo","visible"=>1),
              "jul"=>array("type"=>9,"params"=>"2","label"=>"JUL lb/ac/mo","visible"=>1),
              "aug"=>array("type"=>9,"params"=>"2","label"=>"AUG lb/ac/mo","visible"=>1),
              "sep"=>array("type"=>9,"params"=>"2","label"=>"SEP lb/ac/mo","visible"=>1),
              "oct"=>array("type"=>9,"params"=>"2","label"=>"OCT lb/ac/mo","visible"=>1),
              "nov"=>array("type"=>9,"params"=>"2","label"=>"NOV lb/ac/mo","visible"=>1),
              "dec"=>array("type"=>9,"params"=>"2","label"=>"DEC lb/ac/mo","visible"=>1)
           )
        ),
      "projreaches"=>array(
          "table info"=>array("pk"=>"projectid,reachid", "sortcol"=>"reachid"),
          "column info"=>array(
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0),
              "deletelink"=>array("type"=>15, "params"=>"import_stream_properties.php:projectid:workspace:Delete:actiontype=delete", "label"=>"Delete", "visible"=>0)
           )
       ),
      "loadsum"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0","label"=>"Source Name","visible"=>1, "readonly"=>'1'),
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub-Watershed","visible"=>1, "readonly"=>'1'),
              "production"=>array("type"=>8,"params"=>"0","label"=>"Annual Waste Produced","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualproduced"=>array("type"=>8,"params"=>"0","label"=>"Annual Constituent","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualdieoff"=>array("type"=>8,"params"=>"0","label"=>"Annual Die-off","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualvolatilized"=>array("type"=>8,"params"=>"0","label"=>"Annual Volatilized","visible"=>1, "readonly"=>'0', "width"=>24),
              "annualapplied"=>array("type"=>8,"params"=>"0","label"=>"Annual Applied","visible"=>1, "readonly"=>'0', "width"=>24),
              "sourceload"=>array("type"=>8,"params"=>"0","label"=>"Source Load cfu/Year","visible"=>1, "readonly"=>'0', "width"=>24),
              "dailyload"=>array("type"=>8,"params"=>"0","label"=>"Source Load cfu/Day","visible"=>1, "readonly"=>'0', "width"=>24),
              "pctload"=>array("type"=>7,"params"=>"2","label"=>"Percent Of Total Load","visible"=>1),
              "popdens"=>array("type"=>1,"params"=>"0","label"=>"Population Density","visible"=>1),
              "standpop"=>array("type"=>9,"params"=>"0","label"=>"Standing Population","visible"=>1),
              "totalpop"=>array("type"=>9,"params"=>"0","label"=>"Seasonal/Equivalent Population","visible"=>1),
              "freq_area"=>array("type"=>6,"params"=>"-1:0:N/A","label"=>"Primary habitat","visible"=>1),
              "infreq_area"=>array("type"=>6,"params"=>"-1:0:N/A","label"=>"Secondary Habitat","visible"=>1)
           )
        ),
      "habitat"=>array(
          "table info"=>array("pk"=>"habitatid", "sortcol"=>"habitatid", "outputformat"=>"row"),
          "column info"=>array(
              "habitatid"=>array("type"=>1,"params"=>"","label"=>"Habitat ID","visible"=>0, "readonly"=>'1'),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0, "readonly"=>'1'),
              "sourceid"=>array("type"=>3, "params"=>"sources:sourceid:sourcename:sourcename:0","label"=>"Source Name","visible"=>1, "readonly"=>'1'),
              "sublu"=>array("type"=>1,"params"=>"","label"=>"SUBLU ID","visible"=>1, "readonly"=>'1'),
              "freq_area"=>array("type"=>9,"params"=>"2","label"=>"Primary habitat","visible"=>1),
              "infreq_area"=>array("type"=>9,"params"=>"2","label"=>"Secondary Habitat","visible"=>1),
              "freq_likelihood"=>array("type"=>7,"params"=>"4","label"=>"Primary Freq.","visible"=>1),
              "infreq_likelihood"=>array("type"=>7,"params"=>"4","label"=>"Secondary Freq.","visible"=>1)
           )
        ),
      "groupings"=>array(
          "table info"=>array("pk"=>"grp_id", "sortcol"=>"grp_id", "outputformat"=>"column"),
          "column info"=>array(
              "grp_id"=>array("type"=>1,"params"=>"","label"=>"Group ID","visible"=>0, "readonly"=>'1'),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0, "readonly"=>'1'),
              "groupname"=>array("type"=>1,"params"=>"","label"=>"Group Name","visible"=>1, "readonly"=>'0',"width"=>64),
              "subwatersheds"=>array("type"=>1,"params"=>"","label"=>"Subwatershed/Reach IDs in this group","visible"=>1,"width"=>48),
              "grouptype"=>array("type"=>1,"params"=>"2","label"=>"Group Type","visible"=>1),
              "deletelink"=>array("type"=>15, "params"=>"edit_groupings.php:projectid:workspace:Delete:actiontype=delete", "label"=>"", "visible"=>1)
           )
        ),
      "lusum"=>array(
          "table info"=>array("pk"=>"habitatid", "sortcol"=>"habitatid", "outputformat"=>"row"),
          "column info"=>array(
              "luid"=>array("type"=>1,"params"=>"","label"=>"Land-Use ID","visible"=>0, "readonly"=>'1'),
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0, "readonly"=>'1'),
              "landuse"=>array("type"=>1, "params"=>"","label"=>"Land-Use","visible"=>1, "readonly"=>'0'),
              "luname"=>array("type"=>11, "params"=>"luname:projectid:landuses:hspflu:projectid:landuse:landuse:0","label"=>"Land-Use","visible"=>1, "readonly"=>'0'),
              "luarea"=>array("type"=>9,"params"=>"0","label"=>"Area","visible"=>1, "readonly"=>'1',"halign"=>'right'),
              "lupct"=>array("type"=>7,"params"=>"2","label"=>"% of total Area","visible"=>1,"halign"=>'right'),
              "imppct"=>array("type"=>7,"params"=>"2","label"=>"% of total Area","visible"=>1,"halign"=>'right'),
              "imparea"=>array("type"=>9,"params"=>"2","label"=>"Impervious Area","visible"=>1, "readonly"=>'1',"halign"=>'right')
           )
        ),
      "landclass"=>array(
          "table info"=>array("pk"=>"habitatid", "sortcol"=>"habitatid", "outputformat"=>"row"),
          "column info"=>array(
              "typename"=>array("type"=>1, "params"=>"","label"=>"Land-Use Type","visible"=>1, "readonly"=>'0'),
              "luarea"=>array("type"=>9,"params"=>"0","label"=>"Area","visible"=>1, "readonly"=>'1',"halign"=>'right'),
              "lupct"=>array("type"=>7,"params"=>"2","label"=>"% of total Area","visible"=>1,"halign"=>'right')
           )
        ),
      "agfarm"=>array(
          "table info"=>array("pk"=>"textlabel", "sortcol"=>"textlabel", "outputformat"=>"row"),
          "column info"=>array(
              "textlabel"=>array("type"=>1,"params"=>"","label"=>"Reported Item","visible"=>1, "readonly"=>'0'),
              "total"=>array("type"=>9,"params"=>"0","label"=>"Number","visible"=>1, "readonly"=>'1'),
              "censusyear"=>array("type"=>1, "params"=>"","label"=>"Census Year","visible"=>1, "readonly"=>'0')
           )
        ),
      "int_sources"=>array(
          "table info"=>array("pk"=>"projectid", "sortcol"=>"subshedid"),
          "column info"=>array(
              "projectid"=>array("type"=>1,"params"=>"","label"=>"Project ID","visible"=>0),
              "deletelink"=>array("type"=>15, "params"=>"import_intermittent.php:projectid:workspace:Delete:actiontype=delete", "label"=>"Delete", "visible"=>0),
              "appdate"=>array("type"=>1,"params"=>"","label"=>"Quantity","visible"=>1),
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub-shed ID","visible"=>1),
              "sourceid"=>array("type"=>3, "params"=>"sources:sourceid:sourcename:sourcename:0","label"=>"Source Name","visible"=>1, "readonly"=>'1'),
              "loadapplied"=>array("type"=>1,"params"=>"","label"=>"Quantity","visible"=>1)
           )
       ),
      "intapps"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0","label"=>"Source Name","visible"=>1, "readonly"=>'1'),
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub-Watershed","visible"=>1, "readonly"=>'1'),
              "appdate"=>array("type"=>1,"params"=>"","label"=>"Application Date","visible"=>1, "readonly"=>'1'),
              "cfuapplied"=>array("type"=>8,"params"=>"0","label"=>"Source Load cfu/Day","visible"=>1, "readonly"=>'0', "width"=>24)
           )
        ),
      "sublusource"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "luname"=>array("type"=>1,"params"=>"","label"=>"Land-use","visible"=>1, "readonly"=>'1'),
              "jan"=>array("type"=>8,"params"=>"1","label"=>"January","visible"=>1),
              "feb"=>array("type"=>8,"params"=>"1","label"=>"February","visible"=>1),
              "mar"=>array("type"=>8,"params"=>"1","label"=>"March","visible"=>1),
              "apr"=>array("type"=>8,"params"=>"1","label"=>"April","visible"=>1),
              "may"=>array("type"=>8,"params"=>"1","label"=>"May","visible"=>1),
              "jun"=>array("type"=>8,"params"=>"1","label"=>"June","visible"=>1),
              "jul"=>array("type"=>8,"params"=>"1","label"=>"July","visible"=>1),
              "aug"=>array("type"=>8,"params"=>"1","label"=>"August","visible"=>1),
              "sep"=>array("type"=>8,"params"=>"1","label"=>"September","visible"=>1),
              "oct"=>array("type"=>8,"params"=>"1","label"=>"October","visible"=>1),
              "nov"=>array("type"=>8,"params"=>"1","label"=>"November","visible"=>1),
              "dec"=>array("type"=>8,"params"=>"1","label"=>"December","visible"=>1),
              "annualtotal"=>array("type"=>8,"params"=>"1","label"=>"Annual Total Loas (cfu/yr)","visible"=>1)
           )
        ),
      "watertype"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "classname"=>array("type"=>1,"params"=>"","label"=>"Source Type","visible"=>1, "readonly"=>'1', "width"=>32),
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Reach ID","visible"=>1),
              "jan"=>array("type"=>8,"params"=>"1","label"=>"January","visible"=>1),
              "feb"=>array("type"=>8,"params"=>"1","label"=>"February","visible"=>1),
              "mar"=>array("type"=>8,"params"=>"1","label"=>"March","visible"=>1),
              "apr"=>array("type"=>8,"params"=>"1","label"=>"April","visible"=>1),
              "may"=>array("type"=>8,"params"=>"1","label"=>"May","visible"=>1),
              "jun"=>array("type"=>8,"params"=>"1","label"=>"June","visible"=>1),
              "jul"=>array("type"=>8,"params"=>"1","label"=>"July","visible"=>1),
              "aug"=>array("type"=>8,"params"=>"1","label"=>"August","visible"=>1),
              "sep"=>array("type"=>8,"params"=>"1","label"=>"September","visible"=>1),
              "oct"=>array("type"=>8,"params"=>"1","label"=>"October","visible"=>1),
              "nov"=>array("type"=>8,"params"=>"1","label"=>"November","visible"=>1),
              "dec"=>array("type"=>8,"params"=>"1","label"=>"December","visible"=>1),
              "annualtotal"=>array("type"=>8,"params"=>"1","label"=>"Annual Total Loas (cfu/yr)","visible"=>1)
           )
        ),
      "lucrosstab"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source","visible"=>1, "readonly"=>'1', "width"=>32)
           )
        ),
      "watersource"=>array(
          "table info"=>array("pk"=>"distroid", "sortcol"=>"paramname", "outputformat"=>"row"),
          "column info"=>array(
              "sourcename"=>array("type"=>1,"params"=>"","label"=>"Source","visible"=>1, "readonly"=>'1', "width"=>32),
              "annualtotal"=>array("type"=>8,"params"=>"1","label"=>"Annual Total Loas (cfu/yr)","visible"=>1)
           )
        ),
       "watersheds_dd"=>array(
          "table info"=>array("pk"=>"gid", "sortcol"=>"subshedid", "showascols"=>1),
          "column info"=>array(
              "gid"=>array("type"=>0,"params"=>"","label"=>"GID","visible"=>0, "halign"=>'center', "valign"=>'top'),
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub-watershed ID","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "sources"=>array("type"=>11,"params"=>"projectid:subshedid:subshed:projectid:subshedid:sourceid,popyear,sourcepop:sourceid:1","label"=>"Sources","visible"=>1, "halign"=>'left', "valign"=>'top')
           )
       ),
       "subshed"=>array(
          "table info"=>array("pk"=>"subshedid,projectid", "sortcol"=>"subshedid", "showascols"=>1),
          "column info"=>array(
              "subid"=>array("type"=>0,"params"=>"","label"=>"Subshed Unique ID","visible"=>0, "halign"=>'center', "valign"=>'top'),
              "projectid"=>array("type"=>0,"params"=>"","label"=>"Project ID","visible"=>0, "halign"=>'center', "valign"=>'top'),
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Sub-watershed ID","visible"=>0, "halign"=>'left', "valign"=>'top'),
              "popyear"=>array("type"=>1,"params"=>"","label"=>"Year","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "sourceid"=>array("type"=>3,"params"=>"sources:sourceid:sourcename:sourcename:0","label"=>"Source Name","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "sourcepop"=>array("type"=>1,"params"=>"","label"=>"Population","visible"=>1, "halign"=>'left', "valign"=>'top')
           )
       ),
       "hspf_globals"=>array(
          "table info"=>array("pk"=>"globalid", "sortcol"=>"globalid", "showascols"=>1),
          "column info"=>array(
              "globalid"=>array("type"=>0,"params"=>"","label"=>"Project ID","visible"=>0, "halign"=>'center', "valign"=>'top'),
              "ucifile"=>array("type"=>1,"params"=>"","label"=>"UCI Name","visible"=>0, "halign"=>'left', "valign"=>'top')
           )
       ),
       "map_generic_distro"=>array(
          "table info"=>array("pk"=>"mapid", "sortcol"=>"mapid", "showascols"=>1),
          "column info"=>array(
              "mapid"=>array("type"=>0,"params"=>"","label"=>"Project ID","visible"=>0, "halign"=>'center', "valign"=>'top'),
              "spreadid"=>array("type"=>3, "params"=>"spreadtype:spreadid:spreadname:spreadname:0","label"=>"Spread ID","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "landuseid"=>array("type"=>16, "params"=>"landuses:landuseid:projectid:projectid:landuse:landuse:0","label"=>"Source of Stored Waste","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "apprate"=>array("type"=>1,"params"=>"","label"=>"Application:Need Ratio","visible"=>1, "halign"=>'left', "valign"=>'top'),
              "limpollutant"=>array("type"=>3, "params"=>"pollutanttype:typeid:pollutantname:pollutantname:0","label"=>"Limiting Pollutant","visible"=>1, "halign"=>'left', "valign"=>'top')
           )
        ),
       "cncompare"=>array(
          "table info"=>array("pk"=>"mapid", "sortcol"=>"mapid", "showascols"=>1),
          "column info"=>array(
              "subshedid"=>array("type"=>1,"params"=>"","label"=>"Subshed ID","visible"=>1, "halign"=>'center', "valign"=>'top'),
              "stfips"=>array("type"=>1,"params"=>"","label"=>"Subshed ID","visible"=>1, "halign"=>'center', "valign"=>'top'),
              "pollutanttype"=>array("type"=>1,"params"=>"","label"=>"Subshed ID","visible"=>1, "halign"=>'center', "valign"=>'top'),
              "cropn"=>array("type"=>9,"params"=>"1","label"=>"N Need/acre","visible"=>1),
              "cropp"=>array("type"=>9,"params"=>"1","label"=>"P need/acre","visible"=>1),
              "croparea"=>array("type"=>9,"params"=>"0","label"=>"Land Area","visible"=>1),
              "n_need"=>array("type"=>9,"params"=>"0","label"=>"N need","visible"=>1),
              "p_need"=>array("type"=>9,"params"=>"0","label"=>"P need","visible"=>1),
              "manureapp"=>array("type"=>9,"params"=>"0","label"=>"Manure","visible"=>1),
              "totalfert"=>array("type"=>9,"params"=>"0","label"=>"Total Fertilizer","visible"=>1),
              "nfert"=>array("type"=>9,"params"=>"0","label"=>"Total Nitrogen","visible"=>1),
              "pfert"=>array("type"=>9,"params"=>"0","label"=>"Total Phosphorus","visible"=>1),
              "nrate"=>array("type"=>9,"params"=>"1","label"=>"N rate","visible"=>1),
              "prate"=>array("type"=>9,"params"=>"1","label"=>"P rate","visible"=>1)
           )
        ),
       "bmp_subtypes"=>array(
          "table info"=>array("pk"=>"bmpid", "sortcol"=>"bmptext", "outputformat"=>"row", "rowcolor"=>'white'),
          "column info"=>array(
              "bmpid"=>array("type"=>1,"params"=>"","label"=>"BMP ID","visible"=>0),
              "landuses"=>array("type"=>19,"params"=>"projectid:projectid:projectid:map_landuse_bmp:bmpname:bmpname:luname:landuses:hspflu:landuse","label"=>"Eligible BMPs","visible"=>1, "prefix"=>'', "showchecklabels"=>0),
              "bmpname"=>array("type"=>1,"params"=>"","label"=>"BMP Short Name", "visible"=>1, "width"=>24),
              "bmptext"=>array("type"=>1,"params"=>"","label"=>"Description","visible"=>1,"width"=>50)
           )
        ) ,
      "statsgoprops18_l1"=>array(
          "table info"=>array("pk"=>"oid", "sortcol"=>"muid", "outputformat"=>"row"),
          "column info"=>array(
              "oid"=>array("type"=>1,"params"=>"","label"=>"O ID","visible"=>0, "readonly"=>'1'),
              "muid"=>array("type"=>1, "params"=>"","label"=>"Map Unit","visible"=>1, "readonly"=>'0', "width"=>24),
              "infilt"=>array("type"=>9, "params"=>"4", "label"=>"HSPF Infilt.","visible"=>1, "readonly"=>'0', "width"=>24),
              "depth"=>array("type"=>9,"params"=>"1","label"=>"Depth","visible"=>1, "readonly"=>'0', "width"=>24),
              "sand_pct"=>array("type"=>9,"params"=>"1","label"=>"% Sand","visible"=>1, "readonly"=>'0', "width"=>24),
              "silt_pct"=>array("type"=>9,"params"=>"1","label"=>"% Silt","visible"=>1, "readonly"=>'0', "width"=>24),
              "clay_pct"=>array("type"=>9,"params"=>"1","label"=>"% Clay","visible"=>1, "readonly"=>'0', "width"=>24),
              "om_pct"=>array("type"=>9,"params"=>"1","label"=>"% Organic Matter","visible"=>1, "readonly"=>'0', "width"=>24)
           )
        ),
      "seggroups"=>array(
          "table info"=>array("pk"=>"oid", "sortcol"=>"muid", "outputformat"=>"row"),
          "column info"=>array(
              "oid"=>array("type"=>1,"params"=>"","label"=>"O ID","visible"=>0, "readonly"=>'1'),
              "riverseg"=>array("type"=>1, "params"=>"","label"=>"Outlet Segment","visible"=>1, "readonly"=>'0', "width"=>24),
              "groupname"=>array("type"=>1, "params"=>"4", "label"=>"Station Name","visible"=>1, "readonly"=>'0', "width"=>24),
              "projectid"=>array("type"=>1,"params"=>"1","label"=>"Project ID","visible"=>0, "readonly"=>'0', "width"=>24),
              "thisyear"=>array("type"=>1,"params"=>"1","label"=>"Year","visible"=>1, "readonly"=>'0', "width"=>24),
              "monstation"=>array("type"=>1,"params"=>"1","label"=>"Station ID","visible"=>1, "readonly"=>'0', "width"=>24),
              "pollutanttype"=>array("type"=>3,"params"=>"pollutanttype:typeid:shortname:shortname:0","label"=>"Constituent","visible"=>1, "readonly"=>'0', "width"=>24),
              "total_tons"=>array("type"=>9,"params"=>"1","label"=>"Tons Applied","visible"=>1, "readonly"=>'0', "width"=>24),
              "total_ag"=>array("type"=>9,"params"=>"1","label"=>"Ag. Tons Applied","visible"=>1, "readonly"=>'0', "width"=>24),
              "total_agmanure"=>array("type"=>9,"params"=>"1","label"=>"Ag. Manure Tons Applied","visible"=>1, "readonly"=>'0', "width"=>24),
              "total_agfert"=>array("type"=>9,"params"=>"1","label"=>"Ag. Fertilizer Tons Applied","visible"=>1, "readonly"=>'0', "width"=>24),
              "total_urb"=>array("type"=>9,"params"=>"1","label"=>"Urban Tons Applied","visible"=>1, "readonly"=>'0', "width"=>24),
              "ag_area"=>array("type"=>9,"params"=>"1","label"=>"Ag. Area (acres)","visible"=>1, "readonly"=>'0', "width"=>24),
              "urb_area"=>array("type"=>9,"params"=>"1","label"=>"Urban Area (acres)","visible"=>1, "readonly"=>'0', "width"=>24),
              "total_area"=>array("type"=>9,"params"=>"1","label"=>"Total Area (acres)","visible"=>1, "readonly"=>'0', "width"=>24),
              "ws_area"=>array("type"=>9,"params"=>"1","label"=>"Watershed Area (acres)","visible"=>1, "readonly"=>'0', "width"=>24),
              "urb_perac"=>array("type"=>9,"params"=>"1","label"=>"Urban Rate","visible"=>1, "readonly"=>'0', "width"=>24),
              "ag_perac"=>array("type"=>9,"params"=>"1","label"=>"Ag Rate","visible"=>1, "readonly"=>'0', "width"=>24),
              "total_perac"=>array("type"=>9,"params"=>"1","label"=>"Total Rate","visible"=>1, "readonly"=>'0', "width"=>24),
              "septic_tons"=>array("type"=>9,"params"=>"1","label"=>"Septic (tons)","visible"=>1, "readonly"=>'0', "width"=>24),
              "ps_tons"=>array("type"=>9,"params"=>"1","label"=>"Point Source (tons)","visible"=>1, "readonly"=>'0', "width"=>24)
      )
        )

    );

    $tablecolumns = array(
       "subluparams"=>array(
          'projectid','sublu','luname','luarea','forest','lzsn','infilt','lsur', 'slsur','kvary','agwrc','petmax','petmin','infexp','infild','deepfr','basetp', 'agwetp','cepsc','uzsn','nsur','intfw','irc','lzetp','ceps','surs','uzs','ifws', 'lzs','agws','gwvs','subshedid','paramtype','ripbuffer','sqo','sqolim','retsc', 'stcofips','lucode','parentid', 'catcode', 'maxn', 'maxp', 'pct_nm', 'nm_planbase',  'thisyear'
       ),
       "landuses"=>array(
          'luid', 'landuseid', 'projectid', 'landusetype', 'landuse', 'pct_impervious', 'lzetp', 'cepsc', 'wsqop', 'ioqc', 'aoqc', 'sqolim', 'hspflu', 'parentid'
       ),
       "monthlydistro"=>array(
          'distroid', 'spreadid', 'sourceid', 'sourcetype', 'projectid', 'landuseid', 'distroname', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'parentid'
       ),
       "sourcepollutants"=>array(
          'typeid', 'sourcetypeid', 'pollutantname', 'pollutantconc', 'storagedieoff', 'volatilization', 'concunits', 'conv', 'convunits', 'projectid', 'pollutanttype', 'starttime', 'duration', 'directfraction', 'comments', 'parentid'
       ),
       "sources"=>array(
          'sourceid', 'typeid', 'distrotype', 'projectid', 'sourcename', 'avgweight', 'parentid'
       ),
       "subshed"=>array(
          'subid', 'projectid', 'subshedid', 'sourceid', 'popyear', 'sourcepop', 'parentid'
       ),
       "sourceloadtype"=>array(
          'typeid', 'sourcename', 'auweight', 'pollutantprod', 'produnits', 'pollutantconc', 'storagedieoff', 'concunits', 'conv', 'convunits', 'projectid', 'sourceclass', 'starttime', 'duration', 'directfraction', 'avgweight', 'parentid'
       ),
       "groupings"=>array(
          'grp_id', 'projectid', 'groupname', 'subwatersheds', 'grouptype', 'parentid'
       ),
       "hspf_globals"=>array(
          'globalid', 'projectid', 'ucifile', 'wdm1', 'wdm2', 'wdm3', 'wdm4', 'startdate', 'enddate', 'precip_wdm_id', 'evap_wdm_id', 'uzsn_mo', 'cepsc_mo', 'lzetp_mo', 'reach_wdm_id1', 'reachid1', 'reach_wdm_id2', 'reachid2', 'copyreaches', 'copysubsheds', 'useftablefile', 'ftablefile', 'if', 'ro', 'nsur_mo', 'usethiessen', 'usegqual', 'depwater', 'fcreach', 'trackruns', 'consqual', 'timestep', 'usehydromonfile', 'hydromonfile', 'calcioqcsqolim', 'monioqc', 'allowagwetp', 'impwashoff', 'zerodate', 'parentid'
       ),
       "map_generic_distro"=>array(
          'mapid', 'projectid', 'spreadid', 'landuseid', 'parentid', 'apprate', 'limpollutant'
       )
    );

    # this is a table containing the colums that should be updated in a parent-child relationship
    # generally speaking, these are data only columns.
    $child_columns = array(
       "subluparams"=>array(
          'luname','luarea','forest','lzsn','infilt','lsur', 'slsur','kvary','agwrc','petmax','petmin','infexp','infild','deepfr','basetp', 'agwetp','cepsc','uzsn','nsur','intfw','irc','lzetp','ceps','surs','uzs','ifws', 'lzs','agws','gwvs','subshedid','paramtype','ripbuffer','sqo','sqolim','retsc', 'stcofips','lucode','parentid', 'catcode', 'maxn', 'maxp', 'pct_nm', 'nm_planbase'
       ),
       "landuses"=>array(
          'luid', 'landuseid', 'projectid', 'landusetype', 'landuse', 'pct_impervious', 'lzetp', 'cepsc', 'wsqop', 'ioqc', 'aoqc', 'sqolim', 'hspflu', 'parentid'
       ),
       "monthlydistro"=>array(
          'distroid', 'spreadid', 'sourceid', 'sourcetype', 'projectid', 'landuseid', 'distroname', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'parentid'
       ),
       "sourcepollutants"=>array(
          'pollutantname', 'pollutantconc', 'storagedieoff', 'volatilization', 'concunits', 'conv', 'convunits', 'projectid', 'pollutanttype', 'starttime', 'duration', 'directfraction', 'comments'
       ),
       "sources"=>array(
          'sourceid', 'typeid', 'distrotype', 'projectid', 'sourcename', 'avgweight', 'parentid'
       ),
       "subshed"=>array(
          'sourcepop'
       ),
       "sourceloadtype"=>array(
          'typeid', 'sourcename', 'auweight', 'pollutantprod', 'produnits', 'pollutantconc', 'storagedieoff', 'concunits', 'conv', 'convunits', 'projectid', 'sourceclass', 'starttime', 'duration', 'directfraction', 'avgweight', 'parentid'
       ),
       "groupings"=>array(
          'grp_id', 'projectid', 'groupname', 'subwatersheds', 'grouptype', 'parentid'
       ),
       "hspf_globals"=>array(
          'globalid', 'projectid', 'ucifile', 'wdm1', 'wdm2', 'wdm3', 'wdm4', 'startdate', 'enddate', 'precip_wdm_id', 'evap_wdm_id', 'uzsn_mo', 'cepsc_mo', 'lzetp_mo', 'reach_wdm_id1', 'reachid1', 'reach_wdm_id2', 'reachid2', 'copyreaches', 'copysubsheds', 'useftablefile', 'ftablefile', 'if', 'ro', 'nsur_mo', 'usethiessen', 'usegqual', 'depwater', 'fcreach', 'trackruns', 'consqual', 'timestep', 'usehydromonfile', 'hydromonfile', 'calcioqcsqolim', 'monioqc', 'allowagwetp', 'impwashoff', 'zerodate', 'parentid'
       )
    );

/* end adminsetup array */

?>
