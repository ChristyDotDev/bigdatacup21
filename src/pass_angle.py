from math import atan2, degrees, cos, sin, radians


def angle_between(long1, lat1, long2, lat2):
    result = []
    for i in range(len(lat1)):
        dLon = (long2[i] - long1[i])
        x = cos(radians(lat2[i])) * sin(radians(dLon))
        y = cos(radians(lat1[i])) * sin(radians(lat2[i])) - sin(radians(lat1[i])) * cos(radians(lat2[i])) * cos(
            radians(dLon))
        rads = atan2(x, y)
        degs = degrees(rads)
        result.append(round(degs, 0))
    return result