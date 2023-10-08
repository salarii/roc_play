function outMat = restrictSize(inMAt,val)

outMat = max(inMAt,-val);
outMat = min(outMat,val);