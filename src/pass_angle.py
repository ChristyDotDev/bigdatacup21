from math import atan2, degrees


def angle_between(x1, y1, x2, y2):
    result = []
    for i in range(len(x1)):
        xdiff = x1[i] - x2[i]
        ydiff = y1[i] - y2[i]
        angle_rads = atan2(y2[i] - xdiff, x2[i] - ydiff)
        angle_deg = degrees(angle_rads)
        result.append(round(angle_deg,2))
    return result
