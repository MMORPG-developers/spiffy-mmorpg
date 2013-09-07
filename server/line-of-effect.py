"""
Human madable inconvenient.
Way too sucks.
"""

def get_line(start, end):
    """
    Return a list containing all points between start and end.
    Start and end should be 2-tuples (x, y), given in world coordinates.
    We're assuming x2 >= x1, y2 > y1, and (y2 - y1) <= (x2 - x1)
    """
    
    h_dist_halves = 2 * (end[0] - start[0])
    v_dist_halves = 2 * (end[1] - start[1])
    
    # Calculate the horizontal crossings.
    h_crossings = [0]
    
    for h_progress in range(1, h_dist_halves, 2):
        h_crossings.append(h_progress * v_dist_halves)
    
    # print h_crossings
    
    # Convert to the actual points on the line.
    path = []
    old_v_progress = 0
    next_v_crossing = h_dist_halves
    
    (x, y) = start
    
    for curr_v_progress in h_crossings:
        # Check if we crossed a horizontal gridline
        if curr_v_progress >= next_v_crossing:
            y += 1
            if old_v_progress < next_v_crossing < curr_v_progress:
                path.append((x-1, y))
            next_v_crossing += 2*h_dist_halves
        
        path.append((x, y))
        
        old_v_progress = curr_v_progress
        x += 1
    
    return path


### # def floor_div(numer, denom):
### #     return numer // denom
### # 
### # def ceil_div(numer, denom):
### #     q, r = divmod(numer, denom)
### #     
### #     if r == 0:
### #         return q
### #     else:
### #         return q + 1
### #     
### #     return floor_div(numer + floor_div(denom, 2), denom)
### 
### def div_round_up(numer, denom):
###     """
###     Calculate numer / denom, rounded. In case numer / denom is exactly halfway
###     between two integers, round up.
###     """
###     
###     # return ceil_div(numer + ceil_div(denom, 2), denom)
###     return (numer + (denom // 2)) // denom
### 
### def div_round_down(numer, denom):
###     """
###     Calculate numer / denom, rounded. In case numer / denom is exactly halfway
###     between two integers, round down.
###     """
###     
###     return - div_round_up(- numer, denom)
### 
### print div_round_up(8, 3)
### print div_round_down(8, 3)
### 
### 
### def get_line2(start, end):
###     """
###     Return a list containing all points between start and end.
###     Start and end should be 2-tuples (x, y), given in world coordinates.
###     We're assuming x2 >= x1, y2 > y1, and (y2 - y1) <= (x2 - x1)
###     """
###     
###     h_dist_halves = 2 * (end[0] - start[0])
###     v_dist_halves = 2 * (end[1] - start[1])
###     
###     # Calculate the places where we cross the horizontal gridlines.
###     h_gridline_crossings_scaled = []
###     
###     for v_coord_halves in range(1, v_dist_halves, 2):
###         h_gridline_crossings_scaled.append(v_coord_halves * h_dist_halves)
###     
###     print h_gridline_crossings_scaled
###     
###     path = []
###     
###     # TODO: Handle first row
###     
###     for i in range(len(h_gridline_crossings_scaled) - 1):
###         left_crossing = h_gridline_crossings_scaled[i]
###         right_crossing = h_gridline_crossings_scaled[i+1]
###         
###         row_start = div_round_fuck(left_crossing, v_dist_halves * 2)
###         # row_start = (2 * i + h_dist_halves) / (4 * v_dist_halves)
###     
###     # TODO: Handle last row
###     
###     # We want to divide each of the scaled crossings by v_dist_halves to
###     # convert to the actual number of half-cells, then divide by 2 to convert
###     # to the actual number of whole cells. The result should be rounded to
###     # calculate the correct cell. To save on division, we do this instead.
###     # Whatever this is.
###     # Screw it, I'm tired. Comment better later.
###     h_gridline_crossings = [(2 * x + h_dist_halves) / (4 * v_dist_halves)
###                             for x in h_gridline_crossings_scaled]
###     
###     print h_gridline_crossings
###     
###     raise NotImplementedError
###     
###     # Convert to the actual points on the line.
###     path = []
###     old_v_progress = 0
###     next_v_crossing = h_dist_halves
###     
###     print h_crossings
###     
###     (x, y) = start
###     
###     for curr_v_progress in h_crossings:
###         # Check if we crossed a horizontal gridline
###         if curr_v_progress >= next_v_crossing:
###             y += 1
###             if old_v_progress < next_v_crossing < curr_v_progress:
###                 path.append((x-1, y))
###             next_v_crossing += 2*h_dist_halves
###         
###         path.append((x, y))
###         
###         old_v_progress = curr_v_progress
###         x += 1
###     
###     return path
### 
### 
### 
### 
### 
### 
### 
### 
### 
### # def get_line(start, end):
### #     """
### #     Return a list containing all points between start and end.
### #     Start and end should be 2-tuples (x, y), given in world coordinates.
### #     We're assuming x2 >= x1, y2 > y1, and (y2 - y1) <= (x2 - x1)
### #     """
### #     
### #     # Get the horizontal and vertical distances (in half-cells).
### #     h_dist_halves = 2 * (end[0] - start[0])
### #     v_dist_halves = 2 * (end[1] - start[1])
### #     
### #     # Find the vertical components (in half-cells) of all crossings of
### #     # vertical gridlines, scaled by the horizontal distance (in half-cells).
### #     v_crossing_components = []
### #     v_crossing_component = h_dist_halves
### #     
### #     for h_crossing_component in range(1, h_dist_halves, 2):
### #         v_crossing_components.append(

def draw_path(path):
    x_min = min([thing[0] for thing in path])
    x_max = max([thing[0] for thing in path])
    y_min = min([thing[1] for thing in path])
    y_max = max([thing[1] for thing in path])
    
    pic = []
    
    for y in range(y_min, y_max+1):
        row = ""
        for x in range(x_min, x_max+1):
            if (x, y) == path[0]:
                row += "A"
            elif (x, y) == path[-1]:
                row += "B"
            elif (x, y) in path:
                row += "0"
            else:
                row += " "
        pic.append(row)
    
    return "\n".join(pic)



if __name__ == "__main__":
    path = get_line((0, 0), (17, 13))
    print draw_path(path)

