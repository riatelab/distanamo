SEXP savvy_move_points_from_times__ffi(SEXP c_arg__points, SEXP c_arg__times, SEXP c_arg__factor);

// methods and associated functions for InterpolationGrid
SEXP savvy_InterpolationGrid_new__ffi(SEXP c_arg__source_points, SEXP c_arg__image_points, SEXP c_arg__precision, SEXP c_arg__n_iter, SEXP c_arg__bbox);
SEXP savvy_InterpolationGrid_get_source_grid__ffi(SEXP self__);
SEXP savvy_InterpolationGrid_get_interpolated_grid__ffi(SEXP self__);
SEXP savvy_InterpolationGrid_transform_layer__ffi(SEXP self__, SEXP c_arg__background_layer);
SEXP savvy_InterpolationGrid_deformation_strength__ffi(SEXP self__);
SEXP savvy_InterpolationGrid_sum_squared_deformation_strength__ffi(SEXP self__);
SEXP savvy_InterpolationGrid_resolution__ffi(SEXP self__);
SEXP savvy_InterpolationGrid_bbox__ffi(SEXP self__);
SEXP savvy_InterpolationGrid_mae__ffi(SEXP self__);
SEXP savvy_InterpolationGrid_rmse__ffi(SEXP self__);
SEXP savvy_InterpolationGrid_r_squared__ffi(SEXP self__);