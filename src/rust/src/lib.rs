use distance_cartogram::{Grid, GridType};
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
}
