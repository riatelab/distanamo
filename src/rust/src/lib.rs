use distance_cartogram::{
    generate_positions_from_durations, move_points, CentralTendency, Grid, GridType,
};
use geo_traits::to_geo::ToGeoGeometry;
use geo_types::Coord;
use savvy::{
    savvy, savvy_err, ListSexp, NumericScalar, OwnedListSexp, OwnedRawSexp, RealSexp, Sexp,
    TypedSexp,
};
use wkb::reader::read_wkb;
use wkb::writer::write_geometry;

fn geoms_to_wkb_list(geoms: &[geo_types::Geometry]) -> savvy::Result<OwnedListSexp> {
    let mut out = OwnedListSexp::new(geoms.len(), false)?;

    for (i, geom) in geoms.iter().enumerate() {
        let mut buf = Vec::new();
        write_geometry(&mut buf, &geom, Default::default())?;
        out.set_value(i, OwnedRawSexp::try_from_iter(buf)?)?;
    }

    Ok(out)
}

fn coords_to_points(coords: &[Coord]) -> Vec<geo_types::Geometry> {
    coords
        .iter()
        .map(|c| geo_types::Geometry::Point(geo_types::Point(*c)))
        .collect()
}

fn convert_wkb_point_to_coords(points: ListSexp) -> savvy::Result<Vec<Coord>> {
    points
        .values_iter()
        .map(|v| match v.into_typed() {
            TypedSexp::Raw(rv) => {
                let byte_vector = rv.iter().copied().collect::<Vec<_>>();
                let geom: geo_types::Point<f64> =
                    read_wkb(&byte_vector)?.to_geometry().try_into()?;
                Ok(Coord {
                    x: geom.x(),
                    y: geom.y(),
                })
            }
            _ => Err(savvy_err!("Expected a raw vector")),
        })
        .collect::<savvy::Result<Vec<Coord>>>()
}

fn convert_numericscalar_to_usize(n: NumericScalar, arg_name: &str) -> savvy::Result<usize> {
    let n_usize = n.as_usize();
    if n_usize.is_err() {
        Err(savvy_err!(
            "Expected a positive integer for argument '{}'",
            arg_name
        ))
    } else {
        n_usize
    }
}

#[savvy]
struct InterpolationGrid {
    inner: Grid,
}

#[savvy]
impl InterpolationGrid {
    pub fn new(
        source_points: ListSexp,
        image_points: ListSexp,
        precision: NumericScalar,
        n_iter: NumericScalar,
        bbox: RealSexp,
    ) -> savvy::Result<Self> {
        let source_points = convert_wkb_point_to_coords(source_points)?;
        let image_points = convert_wkb_point_to_coords(image_points)?;

        let bbox = bbox.as_slice();
        if bbox.len() != 4 {
            return Err(savvy_err!("Expected a bbox with 4 elements"));
        }

        let n_iter = convert_numericscalar_to_usize(n_iter, "n_iter")?;

        let grid = Grid::new(
            &source_points,
            &image_points,
            precision.as_f64(),
            n_iter,
            Some((bbox[0], bbox[1], bbox[2], bbox[3]).into()),
        )?;
        Ok(InterpolationGrid { inner: grid })
    }

    pub fn get_source_grid(&self) -> savvy::Result<Sexp> {
        let grid = self
            .inner
            .get_grid(GridType::Source)
            .into_iter()
            .map(geo_types::Geometry::Polygon)
            .collect::<Vec<_>>();

        let out_list = geoms_to_wkb_list(&grid)?;

        Ok(out_list.into())
    }

    pub fn get_interpolated_grid(&self) -> savvy::Result<Sexp> {
        let grid = self
            .inner
            .get_grid(GridType::Interpolated)
            .into_iter()
            .map(geo_types::Geometry::Polygon)
            .collect::<Vec<_>>();

        let out_list = geoms_to_wkb_list(&grid)?;

        Ok(out_list.into())
    }

    pub fn transform_layer(&self, background_layer: ListSexp) -> savvy::Result<Sexp> {
        let background_layer = background_layer
            .values_iter()
            .map(|v| match v.into_typed() {
                TypedSexp::Raw(rv) => {
                    let byte_vector = rv.iter().copied().collect::<Vec<_>>();
                    let geom: geo_types::Geometry<f64> =
                        read_wkb(&byte_vector).unwrap().to_geometry();
                    geom
                }
                _ => panic!("Unexpected input while reading geometries to transform"),
            })
            .collect::<Vec<_>>();

        let bg_transformed = self.inner.interpolate_layer(&background_layer)?;
        let out_list = geoms_to_wkb_list(&bg_transformed)?;

        Ok(out_list.into())
    }

    pub fn transform_layers_parallel(&self, background_layers: ListSexp) -> savvy::Result<Sexp> {
        let background_layers = background_layers
            .values_iter()
            .map(|v| match v.into_typed() {
                TypedSexp::List(l) => {
                    l.values_iter()
                        .map(|v| match v.into_typed() {
                            TypedSexp::Raw(rv) => {
                                let byte_vector = rv.iter().copied().collect::<Vec<_>>();
                                let geom: geo_types::Geometry<f64> =
                                    read_wkb(&byte_vector).unwrap().to_geometry();
                                geom
                            }
                            _ => panic!("Unexpected input while reading geometries to transform"),
                        })
                        .collect::<Vec<_>>()
                }
                _ => panic!("Unexpected input while reading geometries to transform"),
            })
            .collect::<Vec<_>>();

        let bg_transformed = self.inner.interpolate_layers_par(&background_layers)?;

        let mut out_list = OwnedListSexp::new(bg_transformed.len(), true)?;
        for (i, layer) in bg_transformed.iter().enumerate() {
            let l = geoms_to_wkb_list(&layer)?;
            out_list.set_value(i, l)?;
        }

        Ok(out_list.into())
    }

    pub fn deformation_strength(&self) -> savvy::Result<Sexp> {
        self.inner.deformation_strength().try_into()
    }

    pub fn sum_squared_deformation_strength(&self) -> savvy::Result<Sexp> {
        self.inner.sum_squared_deformation_strength().try_into()
    }

    pub fn resolution(&self) -> savvy::Result<Sexp> {
        self.inner.resolution().try_into()
    }

    pub fn bbox(&self) -> savvy::Result<Sexp> {
        let bbox = self.inner.bbox();
        let out: Vec<f64> = vec![bbox.xmin, bbox.ymin, bbox.xmax, bbox.ymax];
        out.try_into()
    }

    pub fn mae(&self) -> savvy::Result<Sexp> {
        self.inner.mae().try_into()
    }

    pub fn rmse(&self) -> savvy::Result<Sexp> {
        self.inner.rmse().try_into()
    }

    pub fn r_squared(&self) -> savvy::Result<Sexp> {
        self.inner.r_squared().try_into()
    }

    pub fn interpolated_points(&self) -> savvy::Result<Sexp> {
        let points = coords_to_points(self.inner.interpolated_points());
        let out_list = geoms_to_wkb_list(&points)?;
        Ok(out_list.into())
    }
}

#[savvy]
fn move_points_from_durations(
    points: ListSexp,
    durations: RealSexp,
    factor: NumericScalar,
) -> savvy::Result<Sexp> {
    let points = convert_wkb_point_to_coords(points)?;

    let new_points = move_points(
        &points,
        durations.as_slice(),
        factor.as_f64(),
        CentralTendency::Mean,
    )?;

    let new_points = coords_to_points(&new_points);
    let out_list = geoms_to_wkb_list(&new_points)?;

    Ok(out_list.into())
}

#[savvy]
fn generate_positions_from_durations_matrix(
    points: ListSexp,
    durations: RealSexp,
) -> savvy::Result<Sexp> {
    let points = convert_wkb_point_to_coords(points)?;

    // We expect the durations matrix to be a square matrix so we only
    // store one dimension number
    let dim = if let Some(dim) = durations.get_dim() {
        if dim.len() != 2 || dim[0] != dim[1] {
            return Err(savvy_err!("Expected a 2D square matrix of durations"));
        }
        if dim[0] != points.len() as i32 {
            return Err(savvy_err!(
                "Number of points and rows in durations matrix do not match"
            ));
        }
        dim[0]
    } else {
        return Err(savvy_err!(
            "No dimensions found for the matrix of durations"
        ));
    };

    // We need to convert the duration matrix (which is column-major)
    // to a row-major Vec of Vecs
    let durations = durations.as_slice();
    let mut durations_matrix = Vec::new();
    for i in 0..dim {
        let mut row = Vec::new();
        for j in 0..dim {
            row.push(durations[(j * dim + i) as usize]);
        }
        durations_matrix.push(row);
    }

    let positioning_result = generate_positions_from_durations(durations_matrix, &points)?;

    let mut out_list = OwnedListSexp::new(2, true)?;

    let points = geoms_to_wkb_list(&coords_to_points(&positioning_result.points))?;

    out_list.set_name_and_value(0, "image_points", points)?;
    out_list.set_name_and_value::<Sexp>(1, "error", positioning_result.error.try_into()?)?;

    Ok(out_list.into())
}
