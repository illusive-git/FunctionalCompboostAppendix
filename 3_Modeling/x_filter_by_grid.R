# this function will filter a dataframe by the variable provided in a grid
# and the row provided by the call

filter_by_grid = function(target, v_grid, v_row){
    # Check stuff
    if(class(dis_1)[1] != "data.table"){
        stop("Error filter_by_grid: Not a data.table")
    }
    if(nrow(v_grid) == 0){
        stop("Error filter_by_grid: Empty grid")
    }
    if(nrow(v_grid) < v_row){
        stop("Error fitler_by_grid: Row out of range")
    }

    # get variable names
    grid_vars = colnames(v_grid)

    # loop through the columns of the grid
    for(k in 1:ncol(v_grid)){
        result = target[get(grid_vars[k]) == v_grid[v_row,k]]
    }

    return(result)
}

