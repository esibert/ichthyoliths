#' validate_code
#'
#' @description
#' The validate_code function checks whether the imported morphology code spreadsheet
#' contains correct values for each character. This is a useful check especially for
#' google sheets imports, which may have copy-paste errors that shuffle dropdowns
#' erroneously between columns.
#'
#' @param code.df is the data frame which contains the morpholgoical character codes
#'
#' @param code.version is a string specifying the version of validation to use.
#'    Current options are c('tooth_v0.5', 'denticle_v0.5')
#'
#' @return Returns a data frame specifying:
#'    The character that is incorrectly coded ("character" column)
#'    The index of the tooth that is incorrectly coded ("index" column)
#'    The value that is currently coded ("incorrect.value" column)
#' This output can be used to then
#'
#' @export


validate_code <- function(code.df, code.version = c('tooth_v0.5, denticle_v0.5')) {

   ##### Validation values for tooth_v0.5 (will eventually have this outside of fn) #####
   if(code.version == 'tooth_v0.5') {
      ## Define the allowed values for each character, including blanks.
      Z1 <- c("Z1.1.Tooth", "Z1.2.Denticle", "Z1.3.Other microfossil", "Z1.4.Excess image/artifact", "")
      Z2 <- c("Z2.1.No fragmentation - outline good", "Z2.2.Some fragmentation - L/W good",
              "Z2.3.Moderate fragmentaiton - traits discernable",
              "Z2.4.Significant fragmentation - can't code", "")
      A1 <- c("A1.0.Too broken to code", "A1.1.Squat", "A1.2.Medium", "A1.3.Long", "A1.4.Extra-long", "")
      A2 <- c("A2.0.Too broken to code", "A2.1.Flat", "A2.2.Mid-depth", "A2.3.Deep", "A2.4.Asymmetrical", "")
      A3 <- c("A3.0.Too broken to code", "A3.1.Triangle", "A3.2.Rectangle", "A3.3.Bulbous", "A3.4.Stalked",
              "A3.5.Half-dome", "A3.6.Oval/Egg", "A3.7.Asymmetrical Top", "A3.8.Asymmetrical Bottom",
              "A3.9.Multi-cusp", "")
      A4 <- c("A4.0.Not a triangle or too broken to code", "A4.1.Straight", "A4.2.Concave", "A4.3.Convex",
              "A4.4.Funnel", "A4.5.Right", "A4.6.Asymmetrical", "")
      B1 <- c("B1.0.Too broken to code", "B1.1.Smooth", "B1.2.Fingerprinted", "B1.3.Furrowed",
              "B1.4.Allen-wrench", "B1.5.Spiraling striation", "B1.6.Spiraling fingerprints",
              "B1.7.Dimpled", "")
      B2 <- c("B2.0.No modifiers or too broken to code", "B2.1.Cloudy", "B2.2.Slinky", "B2.3.Lipped", "")
      B3 <- c("B3.0.No Modifiers or too broken to code", "B3.1.Whole tooth", "B3.2.One side", "B3.3.Tip only",
              "B3.4.Middle only", "B3.5.Bottom only", "")
      C1 <- c("C1.0.Too broken to code", "C1.1.Uncurved", "C1.2.Minor curve", "C1.3.Major curve",
              "C1.4.Recurved", "")
      C2 <- c("C2.0.Uncurved or too broken to code", "C2.1.Whole-tooth", "C2.2.Tip-only", "C2.3.Base-only", "")
      C3 <- c("C3.0.Uncurved or too broken to code", "C3.1.Side-curve", "C3.2.Curl-curve", "C3.3.Multi-curve",
              "C3.4.Cone-curve", "C3.5.Spiral","C3.6.Scoop", "")
      D1 <- c("D1.0.Too broken to code", "D1.1.One-part", "D1.2.Two-part", "D1.3.Three+ parts", "")
      D2 <- c("D2.0.Too broken to code", "D2.1.Cone", "D2.2.Triangle", "D2.3.Rectangle",
              "D2.4.Multi-pronged", "D2.5.Asymmetrical", "")
      E1 <- c("E1.0.Too broken to code", "E1.1.One cusp/Single point", "E1.2.2 cusps", "E1.3.3 cusps",
              "E1.4.4 cusps", "E1.5.5 cusps","E1.6.6 or more cusps", "")
      E2 <- c("E2.0.One cusp or too broken to code", "E2.1.Symmetrical", "E2.2.Asymmetrical", "")
      E3 <- c("E3.0.One cusp or too broken to code", "E3.1.Same size", "E3.2.Middle largest",
              "E3.3.Outer larger", "E3.4.Increasing in size along tooth", "")
      E4 <- c("E4.0.One cusp or too broken to code", "E4.1.Straight", "E4.2.Same direction curve",
              "E4.3.Outer cusps curve out","E4.4.Outer cusps curve in", "")
      E5 <- c("E5.0.One cusp or too broken to code", "E5.1.All cusps same shape", "E5.2.Middle cusp different",
              "E5.3.Outer cusp different", "E5.4.All cusps different", "")
      E6 <- c("E6.0.One cusp or too broken to code", "E6.1.Rounded triangle", "E6.2.Pointed triangle",
              "E6.3.Flat-topped", "E6.4.Dome-shaped", "")
      E7 <- c("E7.0.One cusp or too broken to code", "E7.1.Rounded triangle", "E7.2.Pointed triangle",
              "E7.3.Flat-topped", "E7.4.Dome-shaped", "")
      E8 <- c("E8.0.One cusp or too broken to code", "E8.1.All same size", "E8.2.Largest <2x smallest",
              "E8.3.Largest >2x smallest", "")
      E9 <- c("E9.0.One cusp or too broken to code", "E9.1.Straight", "E9.2.Curved",
              "E9.3.Clustered/Clumped", "")
      F1 <- c("F1.0.Too broken to code", "F1.1.No blades", "F1.2.Symmetrical", "F1.3.Asymmetrical",
              "F1.4.Single", "F1.5.Multi-blade", "F1.6.Different cusps have different blades", "")
      F2 <- c("F2.0.No blades or too broken to code", "F2.1.Small", "F2.2.Medium", "F2.3.Large", "")
      F3 <- c("F3.0.No blades or too broken to code", "F3.1.Upper", "F3.2.Middle", "F3.3.Lower",
              "F3.4.Full-length", "")
      F4 <- c("F4.0.No blades or too broken to code", "F4.1.(1/4)", "F4.2.(1/3)", "F4.3.(1/2)",
              "F4.4.(3/4)", "F4.5.Full-length", "")
      F5 <- c("F5.0.No blades or too broken to code", "F5.1.Straight", "F5.2.Top-heavy",
              "F5.3.Middle-heavy", "F5.4.Bottom-heavy (flared)", "")
      F6 <- c("F6.0.No blades or too broken to code", "F6.1.Straight edge", "F6.2.Outer-curve or long edge",
              "F6.3.Inner-curve or short edge", "")
      F7 <- c("F7.0.No blades or too broken to code", "F7.1.No blade features", "F7.2.Bumps",
              "F7.3.Serrations", "")
      F8 <- c("F8.0.No blades/features or too broken to code", "F8.1.Upper", "F8.2.Middle",
              "F8.3.Lower", "F8.4.Full-length", "")
      G2 <- c("G2.0.No blades or too broken to code", "G2.1.Small", "G2.2.Medium", "G2.3.Large", "")
      G3 <- c("G3.0.No blades or too broken to code", "G3.1.Upper", "G3.2.Middle", "G3.3.Lower",
              "G3.4.Full-length", "")
      G4 <- c("G4.0.No blades or too broken to code", "G4.1.(1/4)", "G4.2.(1/3)", "G4.3.(1/2)",
              "G4.4.(3/4)", "G4.5.Full-length", "")
      G5 <- c("G5.0.No blades or too broken to code", "G5.1.Straight", "G5.2.Top-heavy", "G5.3.Middle-heavy",
              "G5.4.Bottom-heavy (flared)", "")
      G6 <- c("G6.0.No blades or too broken to code", "G6.1.Straight edge", "G6.2.Outer-curve or long edge",
              "G6.3.Inner-curve or short edge", "")
      G7 <- c("G7.0.No blades or too broken to code", "G7.1.No blade features", "G7.2.Bumps",
              "G7.3.Serrations", "")
      G8 <- c("G8.0.No blades/features or too broken to code", "G8.1.Upper", "G8.2.Middle",
              "G8.3.Lower", "G8.4.Full-length", "")
      H1 <- c("H1.0.Too broken to code", "H1.1.No barb(s)", "H1.2.(1/4)", "H1.3.(1/3)", "H1.4.(1/2)",
              "H1.5.(3/4)", "")
      H2 <- c("H2.0.No barb(s)", "H2.1.Top", "H2.2.Middle", "H2.3.Bottom", "")
      H3 <- c("H3.0.No barb(s)", "H3.1.Outer-curve", "H3.2.Inner-curve", "H3.3.Straight-edge", "")
      H4 <- c("H4.0.No barb(s)", "H4.1.Right triangle", "H4.2.Obtuse", "H4.3.Hooked", "")
      I1 <- c("I1.0.No barb(s)", "I1.2.(1/4)", "I1.3.(1/3)", "I1.4.(1/2)", "I1.5.(3/4)", "")
      I2 <- c("I2.0.No barb(s)", "I2.1.Top", "I2.2.Middle", "I2.3.Bottom", "")
      I3 <- c("I3.0.No barb(s)", "I3.1.Outer-curve", "I3.2.Inner-curve", "I3.3.Straight-edge", "")
      I4 <- c("I4.0.No barb(s)", "I4.1.Right triangle", "I4.2.Obtuse", "I4.3.Hooked", "")
      J1 <- c("J1.0.Cannot discern pulp cavity", "J1.1.Rootless", "J1.2.(1/4)", "J1.3.(1/3)",
              "J1.4.(1/2)", "J1.5.(3/4)", "J1.6.Full-length", "")
      J2 <- c("J2.0.Rootless or too broken to code", "J2.1.Thin", "J2.2.Medium", "J2.3.Wide", "J2.4.Total", "")
      J3 <- c("J3.0.Rootless or too broken to code", "J3.1.Straight", "J3.2.Concave", "J3.3.Convex",
              "J3.4.Dome", "J3.5.Funnel", "J3.6.Vase-shape", "J3.7.Flat-top", "J3.8.Asymmetrical", "")
      J4 <- c("J4.0.Rootless/no modifiers or too broken to code", "J4.1.Rough", "J4.2.Branching",
              "J4.3.Extended-tip", "J4.4.Radiating", "")
      K1 <- c("K1.0.Base not preserved", "K1.1.Flat", "K1.2.Flared", "K1.3.Tucked", "K1.4.Inward",
              "K1.5.Pointed", "K1.6.Asymmetrical", "K1.7.Hollow", "")
      L1 <- c("L1.0.One-part tooth or too broken to code", "L1.1.(1/4)", "L1.2.(1/3)", "L1.3.(1/2)",
              "L1.4.(2/3)", "L1.5.(3/4)", "")
      L2 <- c("L2.0.One-part tooth or too broken to code", "L2.1.Straight", "L2.2.Flared", "")
      M1 <- c("M1.0.Tip not preserved/too broken to code", "M1.1.Pointed", "M1.2.Rounded/Dome",
              "M1.3.Flat", "M1.4.Bulbous", "M1.5.Flowery", "M1.6.Rake", "M1.7.Extended tip",
              "M1.8.Protruding", "")
      M2 <- c("M2.0.Tip not preserved/too broken to code", "M2.1.Tip material same as rest of tooth",
              "M2.2.Cap tip", "M2.3.Rim tip", "M2.4.Extended rim tip", "")

      tooth.v0.5.code.list <- list(Z1, Z2, A1, A2, A3, A4, B1, B2, B3, C1, C2, C3, D1, D2,
                                   E1, E2, E3, E4, E5, E6, E7, E8, E9, F1, F2, F3, F4, F5, F6, F7, F8,
                                   G2, G3, G4, G5, G6, G7, G8, H1, H2, H3, H4, I1, I2, I3, I4,
                                   J1, J2, J3, J4, K1, L1, L2, M1, M2)
      names(tooth.v0.5.code.list) <- c('Z1', 'Z2', 'A1', 'A2', 'A3', 'A4', 'B1', 'B2', 'B3',
                                       'C1', 'C2', 'C3', 'D1', 'D2',
                                       'E1', 'E2', 'E3', 'E4', 'E5', 'E6', 'E7', 'E8', 'E9',
                                       'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8',
                                       'G2', 'G3', 'G4', 'G5', 'G6', 'G7', 'G8',
                                       'H1', 'H2', 'H3', 'H4', 'I1', 'I2', 'I3', 'I4',
                                       'J1', 'J2', 'J3', 'J4', 'K1', 'L1', 'L2', 'M1', 'M2')

      ## Export values to the rest of the function:
      # code.list is the list of characters to check
      code.list <- tooth.v0.5.code.list
   }


   ##### Validation values for denticle_v0.5 (will eventually have this outside of fn) #####
   if(code.version == 'denticle_v0.5') {
      # Define the legal values for each character, including blanks.
      Z1 <- c("Z1.1 Tooth", "Z1.2 Denticle", "Z1.3 Other microfossil", "Z1.4 Excess image/artifact", "")
      A1 <- c("A1.0 Too broken to code", "A1.1 Spine", "A1.2 Cruciform", "A1.3 Circular or oval",
              "A1.4 Spade", "A1.5 Diamond/Rectangle/(Can be asymmetrical/kite-like)",
              "A1.6 Elongated Asymmetrical", "A1.7 Irregular/ other", 'A1.8 “Fan-like”',
              "A1.9 Triangular or Arrow-like", "")
      A2 <- c("A2.0 Not a Spade/too broken to code", "A2.1 Rounded Spade", "A2.2 Squared Spade",
             "A2.3 Pointed Spade", "A2.4 Stretched Spade", "A2.5 Lobed Spade", "A2.6 Irregular", "")
      B1 <- c("B1.0 Too broken to code", "B1.1 Straight (edge)", "B1.2 Rounded", "B1.3 Pointed (vertex)",
              "B1.4 Denticle lacks directionality/Cannot discern anterior", "")
      B2 <- c("B2.0 Too broken to code", "B2.1 Smooth", "B2.2 Scalloped margin texture",
              "B2.3 Pointed margin texture", "B2.4 anterior is a vertex", "")
      B3 <- c("B3.0 Too broken to code", "B3.1 Smooth", "B3.2 Distinct serrated", "")
      C1 <- c("C1.0 Too broken to code", "C1.1 Straight", "C1.2 Rounded", "C1.3 Pointed",
              "C1.4 Denticle lacks directionality/Cannot discern posterior", "")
      C2 <- c("C2.0 Too broken to code", "C2.1 Smooth", "C2.2 Scalloped margin texture",
              "C2.3 Pointed margin texture", "C2.4 Posterior is a vertex", "")
      C3 <- c("C3.0 Too broken to code", "C3.1 Smooth", "C3.2 Distinct serrated", "")
      D1 <- c("D1.0 Too broken to code", "D1.1 None", "D1.2 one", "D1.3 two", "D1.4 radial", "")
      E1 <- c("E1.0 Too broken to code", "E1.1 None", "E1.2 one", "E1.3 two", "E1.4 three",
              "E1.5 four", "E1.6 five", "E1.7 six", "E1.8 seven", "E1.9 eight", "E1.10 nine",
              "E1.11 ten or more", "")
      E2 <- c("E2.0 No cusps or too broken to code", "E2.1 Ridges not associated with cusps",
              "E2.2 Cusps defined by ridges", "E2.3 Some cusps defined by ridges", "")
      E3 <- c("E3.0 No cusps/one cusp or too broken to code", "E3.1 Cusps are same length/width/shape",
              "E3.2 Central cusp is distinct",
              "E3.3 cusps opposite each other are similar, cusps next to each other are distinct",
              "E3.4 Irregular", "")
      E4 <- c("E4.0 No cusps or too broken to code", "E4.1 x < ¼ ", "E4.2 ¼ <x<½", "E4.3 ½ < x", "")
      F1 <- c("F1.0 Too broken to code", "F1.1 Smooth",
              "F1.2 Linear (can tell directionality-anterior/posterior, ridges only curve in a max. 1 way-not wavy)",
              "F1.3 Geometric (Central branching pattern)",
              "F1.4 Meandering ridges (ridges curve or intersect in more than 1 way/direction)",
              "F1.5 Spine",
              "F1.6 Branching (can tell directionality but ridges curve or intersect in more than 1 way)",
              "")
      G1 <- c("G1.0 Too broken to code", "G1.1 None", "G1.2 one", "G1.3 two", "G1.4 three",
              "G1.5 four", "G1.6 five", "G1.7 six-ten", "G1.8 eleven or more", "")
      G2 <- c("G2.0 Too broken to code", "G2.1 None", "G2.2 one", "G2.3 two", "G2.4 three",
              "G2.5 four", "G2.6 five", "G2.7 six or more", "")
      G3 <- c("G3.0 no ridges or too broken to code", "G3.1 no ridge outgrowths",
              "G3.2 one ridge outgrowth", "G3.3 two ridge outgrowths", "")
      H1 <- c("H1.0 Only one Ridge/No Ridges or too broken to code", "H1.1 Parallel",
              "H1.2 Converges from anterior", "H1.3 Diverges from anterior",
              "H1.4 Diverges then converges", "H1.5 Ridges intesect/branch",
              "H1.6 Apex Radial (spine)", "H1.7 Irregular/no discernable anterior/posterior",
              "H1.8 multiple", "")
      I1 <- c("I1.0 No ridges or too broken to code", "I1.1 No central ridge",
              "I1.2 Only one Ridge", "I1.3 Same as other ridges",
              "I1.4 Distinct shape/size from other ridges", "")
      I2 <- c("I2.0 None or too broken to code", "I2.1 Straight", "I2.2 Curved",
              "I2.3 Meandering", "")
      I3 <- c("I3.0 None or too broken to code", "I3.1 Parallel",
              "I3.2 Triangular widening from anterior",
              "I3.3 Triangular thinning from anterior", "I3.4 Diamond-like",
              "I3.5 Creates a Central Trough", "I3.6 Irregular", "")
      J1 <- c("J1.0 None/only one ridge or too broken to code", "J1.1 Straight",
              "J1.2 Concave Curved", "J1.3 Convex Curved", "J1.4 Meandering",
              "J1.5 Combination", "")
      J2 <- c("J2.0 None or too broken to code", "J2.1 Parallel",
              "J2.2 Triangular widening from anterior",
              "J2.3 Triangular thinning from anterior", "J2.4 Diamond-like",
              "J2.5 Creates Troughs", "J2.6 Irregular/ Combination", "")
      K1 <- c("K1.0 No Ridges or too broken to code",
              "K1.1 Ridges present but no central shape observed",
              "K1.2 Circular/oval", "K1.3 Triangular", "K1.4 Quadrilateral",
              "K1.5 Pentagonal", "K1.6 Hexagon", "K1.7 Heptagon", "K1.8 Mound",
              "K1.9 Octagon", "K1.10 Multiple", "K1.11 Irregular", "")
      K2 <- c("K2.0 No central shape or too broken to code",
              "K2.1 central shape but no plane of symmetry", "K2.2 one",
              "K2.3 two", "K2.4 radial", "")
      L1 <- c("L1.0 Too broken to code", "L1.1 No ridges",
              "L1.2 Ridge length=lateral length",
              "L1.3 Ridge ends mid crown/ Ridge begins mid crown",
              "L1.4 Ridge begins and ends mid crown",
              "L1.5 Includes ridges described by a combination of 1,2, 3,", "")
      L2 <- c("L2.0 No Ridges or too broken to code", "L2.1 Not Clearly Defined",
              "L2.2 Clearly defined on part of the crown but gets shallower and does not extend to full length of denticle",
              "L2.3 Clearly Defined",
              "L2.4 Only the central ridge is clearly defined ", "")
      L3 <- c("L3.0 None or too broken to code", "L3.1 Only one ridge",
              "L3.2 equal ridge heights", "L3.3 Variable ridge height", "")
      L4 <- c("L4.0 No ridges or too broken to code", "L4.1 Ridge profiles or trough are rounded",
              "L4.2 Ridge profiles or troughs are triangular",
              "L4.3 Ridge profiles or troughs are funnel shapes (rounded with a skinnier point)",
              "L4.4 Ridge profiles or troughs are variable or described by another shape",
              "L4.5 Ridge profiles or troughs are rectangular", "")
      M1 <- c("M1.0 Too broken to code", "M1.1 None", "M1.2 one", "M1.3 two", "M1.4 three",
              "M1.5 four", "M1.6 five or more",
              "M1.7 Multiple depressions (>2) but denticle is broken so unable to discern precise number",
              "")
      M2 <- c("M2.0 No depression or too broken to code", "M2.1 Thumbprint (surface layer)",
              "M2.2 Open tunnel (partially covered)",
              "M2.3 Dimples (depressions surrounded by ridges)", "")
      M3 <- c("M3.0 No depression or too broken to code", "M3.1 Central depression",
              "M3.2 Close to edge depression", "M3.3 Both central and edge depressions", "")
      M4 <- c("M4.0 none or too broken to code", "M4.1 circle", "M4.2 Elongated",
              "M4.3 Teardrop (smooth curve)", "M4.4 Square/quadrilateral", "M4.5 Pentagon",
              "M4.6 Irregular", "M4.7 Multiple different shapes", "")
      N1 <- c("N1.0 Too broken to code", "N1.1 No Ridges",
              "N1.2 No secondary ridge features", "N1.3 Micro-reliefs on ridges",
              "N1.4 Honeycomb surface texture", "N1. 5 Wavy surface texture",
              "N1.6 Honeycomb/wavey combination surface texture",
              "N1.7 Vertical protrusion(s) on ridges", "N1.8 Pockmarks", "")
      N2 <- c("N2.0 None or too broken to code", "N2.1 Ridges only", "N2.2 Crown and ridges",
              "N2.3 Crown except ridges", "N2.4 Anterior of crown only",
              "N2.5 Middle of crown only", "N2.6 Posterior of crown only",
              "N2.7 Edges of crown only", "")
      N3 <- c("N3.0 None or too broken to code", "N3.1 Less than 30%", "N3.2 Between 30% and 70%",
              "N3.3 Greater than 70%", "")
      O1 <- c("O1.0 Base not preserved/too broken to code", "O1.1 Kite shaped",
              "O1.2 Kite/cruciform shaped",
              "O1.3 Kite/cruciform shaped missing 1/4 of the cross ridges/extensions",
              "O1.4 Rhombus shaped", "O1.5 Rounded rhombus",
              "O1.6 Stretched rhombus",
              "O1.7 Trapazoid/rhombus hybrid (rhombus like but with a flat anterior edge)",
              "O1.8 Oval/oval like", "O1.9 Circular",
              "O1.10 Tree roots (irregular with many radiating ridges)",
              "O1.11 Mirrors crown shape",
              "O1.12 Base mirrors crown shape and there is no distinct separation between crown and base (no peduncle)", "")
      O2 <- c("O2.0 Base not preserved/too broken to code", "O2.1 Equal width and length",
              "O2.2 Wider than long", "O2.3 Longer than wide",
              "O2.4 Unequal width and length (no directionality)", "")
      O3 <- c("O3.0 Base not preserved/too broken to code", "O3.1 Crown and root have the same area",
              "O3.2 Crown has a larger area", "O3.3 Base has a larger area", "")
      O4 <- c("O4.0 Base not preserved/too broken to code", "O4.1 No grooves, completely rounded/smooth",
              "O4.2 One", "O4.3 Two", "O4.4 Three", "O4.5 Four", "O4.6 Five", "O4.7 Six", "")
      O5 <- c("O5.0 Base not preserved/too broken to code", "O5.1 No root opening",
              "O5.2 Rhombus", "O5.3 Elipse", "O5.4 Arc",
              "O5.5 Mirrors base shape with more rounded edges", "")
      O6 <- c("O6.0 No root opening or base not preserved/too broken to code",
              "O6.1 Center of base", "O6.2 Anterior of base", "O6.3 Posterior of base", "")
      O7 <- c("O7.0 Base not preserved/too broken to code", "O7.1 Equal width and height",
              "O7.2 Wider than height", "O7.3 Higher than width",
              "O7.4 No Peduncle (sits flat on surface)", "")
      O8 <- c("O8.0 Base not preserved/too broken to code", "O8.1 Perpendicular",
              "O8.2 Obtuse", "O8.3 Parallel", "O8.4 Acute", "")
      O9 <- c("O9.0 Base not preserved/too broken to code", "O9.1 at anterior of crown",
              "O9.2 at center of crown", "O9.3 at posterior of crown", "")
      O10 <- c("O10.0 Base not preserved/too broken to code", "O10.1 Not mounded",
               "O10.2 Mounded", "")

      denticle.v0.5.code.list <- list(Z1, A1, A2, B1, B2, B3, C1, C2, C3, D1,
                                      E1, E2, E3, E4, F1, G1, G2, G3, H1,
                                      I1, I2, I3, J1, J2, K1, K2, L1, L2, L3, L4,
                                      M1, M2, M3, M4, N1, N2, N3,
                                      O1, O2, O3, O4, O5, O6, O7, O8, O9, O10)
      names(denticle.v0.5.code.list) <- c('Z1', 'A1', 'A2', 'B1', 'B2', 'B3',
                                          'C1', 'C2', 'C3', 'D1', 'E1', 'E2', 'E3', 'E4',
                                          'F1', 'G1', 'G2', 'G3', 'H1', 'I1', 'I2', 'I3',
                                          'J1', 'J2', 'K1', 'K2', 'L1', 'L2', 'L3', 'L4',
                                          'M1', 'M2', 'M3', 'M4', 'N1', 'N2', 'N3',
                                          'O1', 'O2', 'O3', 'O4', 'O5', 'O6', 'O7', 'O8', 'O9', 'O10')
      code.list <- denticle.v0.5.code.list
   }

   ##### Extract coded columns based on code list values: #####

   ## code.cols are the columns of the initial database to check.
   cols.names <- colnames(code.df)
   code.start.col <- which(cols.names == head(names(code.list), 1))[1] #First value of code.list
   code.end.col <- which(cols.names == tail(names(code.list), 1))[1] #Last value of code.list
   code.cols <- code.df[,code.start.col:code.end.col] #data frame of code columns only

   ## Confirm that code.cols and code.list have the same length. If they do not,
   #     throw an error specifying that there are missing columns.
   if(length(code.list) != dim(code.cols)[2]) {

      ## comparing code.list names with code.cols names,
      # Asking "which code.cols is not in the code list?"
      missing.cols <- names(code.list)[which(!names(code.list) %in% names(code.cols))]

      stop(paste(c("Code is mising columns:", missing.cols, ",", "cannot continue"), sep = '', collapse = " "))
   }


   ##### Evaluate for impossible code values #####

   # Empty data frame to fill:
   impossible.code.df <- data.frame(character = character(0),
                                    index = numeric(0),
                                    incorrect.value = character(0))

   # Run through possible incorrect codes
   for(i in 1:length(code.list)) {
      test.list <- c(code.list[[i]]) #extract as vector
      incorrect.codes <- which(code.cols[,i] %in% test.list == FALSE)
      if(length(incorrect.codes >0)) {
         temp.df <- data.frame(character = names(code.list[i]),
                               index = incorrect.codes,
                               incorrect.value = code.cols[incorrect.codes, i])
         impossible.code.df <- rbind(impossible.code.df, temp.df)
      }
   }

   return(impossible.code.df)

}


