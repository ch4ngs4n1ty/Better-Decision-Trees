def classify(HemHt, BowTieWd):
    if BowTieWd < 5.510:
        return "Guilder"
    else:
        if BowTieWd < 5.595:
            return "Guilder"
        else:
            if BowTieWd < 5.620:
                return "Guilder"
            else:
                if HemHt < 8.895:
                    if BowTieWd < 7.860:
                        return "Guilder"
                    else:
                        if HemHt < 6.825:
                            if BowTieWd < 9.830:
                                return "Guilder"
                            else:
                                if BowTieWd < 12.075:
                                    if HemHt < 5.200:
                                        if HemHt < 4.725:
                                            if HemHt < 4.640:
                                                if HemHt < 4.540:
                                                    if HemHt < 4.360:
                                                        return "Guilder"
                                                    else:
                                                        if HemHt < 4.470:
                                                            return "Florin"
                                                        else:
                                                            return "Guilder" 
                                                else:
                                                    return "Guilder"
                                            else:
                                                return "Guilder"
                                        else:
                                            return "Guilder"
                                    else:
                                        return "Florin"
                                else:
                                    return "Florin"
                        else:
                            return "Florin"
                else:
                    return "Florin"
