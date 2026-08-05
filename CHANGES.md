This file describes changes in the Cubefree package.

## 1.22 (2026-08-06)

  - Speed up `NumberCFGroups` and `NumberCFSolvableGroups` by two orders of
    magnitude, and cache their data across calls
  - Convert the manual to GAPDoc/AutoDoc
  - Various janitorial changes

## 1.21 (2025-08-29)

  - Declare the SmallGrp package dependency explicitly and require GAP 4.9
  - Various janitorial changes

## 1.20 (2024-11-01)

  - Speed up the cubefree isomorphism test by comparing polynomials instead
    of their companion matrices
  - Various janitorial changes

## 1.19 (2022-02-21)

  - Fix building the manual with recent GAP versions
  - Various janitorial changes

## 1.18 (2019-09-30)

  - Fix `IsomorphismCubefreeGroups` for the trivial group
  - Rename `IsIsomorphismCubefreeGroups` to `IsIsomorphicCubefreeGroups`
  - Improve `IsomorphismCubefreeGroups` and its `NC` variant
  - Raise the minimum versions to GAP 4.8, GrpConst 2.5, and Polycyclic 2.11
  - Move the package to its current repository and documentation setup
  - Various janitorial changes

## 1.17 (2018-06-11)

  - Add `IsomorphismCubefreeGroupsNC` and `IsIsomorphismCubefreeGroups`
  - Rework and speed up the isomorphism test for cubefree groups

## 1.16 (2016-09-06)

  - Add `IsomorphismCubefreeGroups` to construct an isomorphism between two
    cubefree groups

## 1.15 (2015-12-19)

  - Various janitorial changes

## 1.14 (2015-12-17)

  - Fix the construction of Frattini-free groups for the prime 2 and use
    minimal generating sets for cyclic automorphism groups

## 1.13 (2012-12-10)

  - Various janitorial changes

## 1.12 (2012-12-03)

  - Update the maintainer contact details and package URLs

## 1.11 (2012-06-16)

  - Various janitorial changes

## 1.10 (2012-05-07)

  - Fix loading the package with newer GAP versions, which no longer provide
    `DeclarePackage` and `RequirePackage`
  - Update the maintainer contact details and package URLs

## 1.08 (2007-10-22)

  - Update the package metadata after acceptance as a GAP package

## 1.07 (2007-10-17)

  - Add `CubefreeOrderInfo`, which estimates the complexity of constructing
    the groups of a given cubefree order
  - Add `CubefreeTestOrder` to compare the constructed groups with the
    SmallGroups library
  - Rework `IrreducibleSubgroupsOfGL` and the counting functions, and use the
    SmallGroups library where it is available
  - Declare the Polycyclic package dependency

## 1.05 (2007-05-08)

  - Improve the documentation of the package functions
  - Various janitorial changes

## 1.04 (2006-05-05)

  - Shrink the precomputed data for the reducible subgroups of `GL(2,p)`
    considerably by storing them over the prime field
  - Improve the construction of irreducible subgroups of `GL(2,q)` and of
    Frattini-free groups

## 1.03 (2006-02-25)

  - Various janitorial changes

## 1.01 (2006-02-11)

  - Various janitorial changes

## 1.0 (2005-12-09)

  - Initial release
