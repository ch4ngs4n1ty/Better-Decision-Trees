def classify_spy(HemHt, BowTieWd):
    if HemHt < 5.510:
        return "Guilder"
    else:
        if HemHt < 5.595:
            return "Guilder"
        else:
            if HemHt < 5.620:
                return "Guilder"
            else:
                if BowTieWd < 8.895:
                    if HemHt < 7.860:
                        return "Guilder"
                    else:
                        if BowTieWd < 6.825:
                            if HemHt < 9.830:
                                return "Guilder"
                            else:
                                if HemHt < 12.075:
                                    if BowTieWd < 5.200:
                                        if HemHt < 10.410:
                                            return "Guilder"
                                        else:
                                            if HemHt < 10.465:
                                                return "Guilder"
                                            else:
                                                if HemHt < 10.520:
                                                    return "Guilder"
                                                else:
                                                    if HemHt < 10.650:
                                                        if HemHt < 10.585:
                                                            return "Guilder"
                                                        else:
                                                            if BowTieWd < 4.360:
                                                                return "Guilder"
                                                            else:
                                                                return "Florin"  # Pure node: 100.0% Florin
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
