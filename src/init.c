
#include <stdint.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

#include "rust/api.h"

static uintptr_t TAGGED_POINTER_MASK = (uintptr_t)1;

SEXP handle_result(SEXP res_) {
    uintptr_t res = (uintptr_t)res_;

    // An error is indicated by tag.
    if ((res & TAGGED_POINTER_MASK) == 1) {
        // Remove tag
        SEXP res_aligned = (SEXP)(res & ~TAGGED_POINTER_MASK);

        // Currently, there are two types of error cases:
        //
        //   1. Error from Rust code
        //   2. Error from R's C API, which is caught by R_UnwindProtect()
        //
        if (TYPEOF(res_aligned) == CHARSXP) {
            // In case 1, the result is an error message that can be passed to
            // Rf_errorcall() directly.
            Rf_errorcall(R_NilValue, "%s", CHAR(res_aligned));
        } else {
            // In case 2, the result is the token to restart the
            // cleanup process on R's side.
            R_ContinueUnwind(res_aligned);
        }
    }

    return (SEXP)res;
}

SEXP savvy_move_points_from_times__impl(SEXP c_arg__points, SEXP c_arg__times, SEXP c_arg__factor) {
    SEXP res = savvy_move_points_from_times__ffi(c_arg__points, c_arg__times, c_arg__factor);
    return handle_result(res);
}

SEXP savvy_InterpolationGrid_new__impl(SEXP c_arg__source_points, SEXP c_arg__image_points, SEXP c_arg__precision, SEXP c_arg__n_iter, SEXP c_arg__bbox) {
    SEXP res = savvy_InterpolationGrid_new__ffi(c_arg__source_points, c_arg__image_points, c_arg__precision, c_arg__n_iter, c_arg__bbox);
    return handle_result(res);
}

SEXP savvy_InterpolationGrid_get_source_grid__impl(SEXP self__) {
    SEXP res = savvy_InterpolationGrid_get_source_grid__ffi(self__);
    return handle_result(res);
}

SEXP savvy_InterpolationGrid_get_interpolated_grid__impl(SEXP self__) {
    SEXP res = savvy_InterpolationGrid_get_interpolated_grid__ffi(self__);
    return handle_result(res);
}

SEXP savvy_InterpolationGrid_transform_layer__impl(SEXP self__, SEXP c_arg__background_layer) {
    SEXP res = savvy_InterpolationGrid_transform_layer__ffi(self__, c_arg__background_layer);
    return handle_result(res);
}

SEXP savvy_InterpolationGrid_deformation_strength__impl(SEXP self__) {
    SEXP res = savvy_InterpolationGrid_deformation_strength__ffi(self__);
    return handle_result(res);
}

SEXP savvy_InterpolationGrid_sum_squared_deformation_strength__impl(SEXP self__) {
    SEXP res = savvy_InterpolationGrid_sum_squared_deformation_strength__ffi(self__);
    return handle_result(res);
}

SEXP savvy_InterpolationGrid_resolution__impl(SEXP self__) {
    SEXP res = savvy_InterpolationGrid_resolution__ffi(self__);
    return handle_result(res);
}

SEXP savvy_InterpolationGrid_bbox__impl(SEXP self__) {
    SEXP res = savvy_InterpolationGrid_bbox__ffi(self__);
    return handle_result(res);
}


static const R_CallMethodDef CallEntries[] = {
    {"savvy_move_points_from_times__impl", (DL_FUNC) &savvy_move_points_from_times__impl, 3},
    {"savvy_InterpolationGrid_new__impl", (DL_FUNC) &savvy_InterpolationGrid_new__impl, 5},
    {"savvy_InterpolationGrid_get_source_grid__impl", (DL_FUNC) &savvy_InterpolationGrid_get_source_grid__impl, 1},
    {"savvy_InterpolationGrid_get_interpolated_grid__impl", (DL_FUNC) &savvy_InterpolationGrid_get_interpolated_grid__impl, 1},
    {"savvy_InterpolationGrid_transform_layer__impl", (DL_FUNC) &savvy_InterpolationGrid_transform_layer__impl, 2},
    {"savvy_InterpolationGrid_deformation_strength__impl", (DL_FUNC) &savvy_InterpolationGrid_deformation_strength__impl, 1},
    {"savvy_InterpolationGrid_sum_squared_deformation_strength__impl", (DL_FUNC) &savvy_InterpolationGrid_sum_squared_deformation_strength__impl, 1},
    {"savvy_InterpolationGrid_resolution__impl", (DL_FUNC) &savvy_InterpolationGrid_resolution__impl, 1},
    {"savvy_InterpolationGrid_bbox__impl", (DL_FUNC) &savvy_InterpolationGrid_bbox__impl, 1},
    {NULL, NULL, 0}
};

void R_init_distanamo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    // Functions for initialzation, if any.

}
