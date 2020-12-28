# Revision history for subG

## 0.1.0.0 -- 2020-10-16

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2020-10-16

* First version revised A. Discontinued the support for the GHC-7.8.* series.

## 0.1.1.1 -- 2020-10-16

* First version revised B. Some documentation improvements.

## 0.2.0.0 -- 2020-11-17

* Second version. Added new functions to the Data.SubG module. Added a new module Data.MinMax with the functions that allow to find out both minimum and
maximum elements of the Foldable structures.

## 0.2.1.0 -- 2020-11-18

* Second version revised A. Fixed issues with the ambiguous names in the further reverse dependencies on the package. Now the ambiguous functions have
in their names suffix G (it means 'generalization'). Added a new function splitAtEndG to the Data.SubG module. Fixed issues with multiple search minimum and
maximum elements functions in the module Data.MinMax. Allowed equal elements in the structures so they are now general. Some documentation improvements.

## 0.3.0.0 -- 2020-11-18

* Third version. Added a new module Data.MinMax3Plus with additional functions.

## 0.4.0.0 -- 2020-11-18

* Fourth version. Added new modules Data.MinMax3Plus.Preconditions and Data.MinMax.Preconditions with additional functions. Added -By functions to the modules
to make them more general. The modules with Preconditions in the names just use functions with no checking the needed length of the structure.

## 0.4.1.0 -- 2020-11-19

* Fourth version revised A. Added two new functions mapG and filterG to the module.

## 0.4.2.0 -- 2020-11-19

* Fourth version revised B. Added two new functions partitionG to the module.
