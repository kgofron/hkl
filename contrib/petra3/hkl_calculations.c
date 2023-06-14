#include <stdio.h>

#include "hkl.h"
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>

double a = 4.542, b = 16.955, c = 7.389;
double alpha = 90.0 * HKL_DEGTORAD;
double beta = 90.0 * HKL_DEGTORAD;
double gamm = 90.0 * HKL_DEGTORAD;

char *diffr_type = "E6C";
double wavelength = 1.62751693358;
double angles_or0[] = {0.0, 22.31594, 89.1377, 0.0, 0.0, 45.15857};
double hkl_or0[] = {0, 8, 0};
double angles_or1[] = {0.0, 34.96232, 78.3139, 0.0, 0.0, 71.8007};
double hkl_or1[] = {0, 12, 1};
double ux = 0.878929693221, uy = -3.6132870009, uz = 0.263869539307;
char *engine_type = "hkl";
char *engine_mode = "constant_phi_vertical";

int n_refl = 9;
size_t n_angles = sizeof(angles_or0)/sizeof(angles_or0[0]);

HklGeometry *new_hkl_geometry(char *dtype, double wavelength)
{
    HklFactory *factory;
    HklGeometry *geometry;

    factory = hkl_factory_get_by_name(dtype, NULL);
    geometry = hkl_factory_create_new_geometry(factory);
    if(TRUE != hkl_geometry_axis_values_set(geometry, angles_or0, n_angles, HKL_UNIT_USER, NULL)){
        printf("\n>>> There is a problem during the creation of the new geometry.\n");
        exit(EXIT_FAILURE);
    }
    if(TRUE != hkl_geometry_wavelength_set(geometry, wavelength, HKL_UNIT_USER, NULL)){
        printf("\n>>> There is a problem during the assignment of the wavelength.\n");
        exit(EXIT_FAILURE);
    }

    return geometry;
}

HklSampleReflection *new_sample_reflection(HklGeometry *geometry, HklDetector *detector,
            double angles_refl[], size_t n_angles, double hkl_refl[3], HklSample *sample)
{
    HklSampleReflection *reflection;

    if(TRUE != hkl_geometry_axis_values_set(geometry, angles_refl, n_angles, HKL_UNIT_USER, NULL)){
        printf("\n>>> There is a problem with the angles of the reflection %5.3f %5.3f %5.3f.\n",
                hkl_refl[0], hkl_refl[1], hkl_refl[2]);
        exit(EXIT_FAILURE);
    }
	reflection = hkl_sample_reflection_new(geometry, detector, hkl_refl[0], hkl_refl[1], hkl_refl[2], NULL);
	hkl_sample_add_reflection(sample, reflection);

    return reflection;
}

HklSample *new_hkl_sample(void)
{
    HklSample *sample;
    HklLattice *lattice;
    HklGeometry *geometry;
    HklDetector *detector;
    HklSampleReflection *or0, *or1;

    sample = hkl_sample_new("sample name");
    lattice = hkl_lattice_new(a, b, c, alpha, beta, gamm, NULL);
    hkl_sample_lattice_set(sample, lattice);

    geometry = new_hkl_geometry(diffr_type, wavelength);

    detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

    or0 = new_sample_reflection(geometry, detector, angles_or0, n_angles, hkl_or0, sample);
    or1 = new_sample_reflection(geometry, detector, angles_or1, n_angles, hkl_or1, sample);
	if(TRUE != hkl_sample_compute_UB_busing_levy(sample, or0, or1, NULL)){
	    printf("\n>>> There is a problem during the calculation of the UB matrix.\n");
        exit(EXIT_FAILURE);
    }

    /* Another way of obtaining the UB matrix: using the parameters Ux, Uy and Uz . */
    /*
    parameter = hkl_sample_ux_get(sample);
    if(TRUE != hkl_parameter_value_set(parameter, ux, HKL_UNIT_USER, NULL) ||
       TRUE != hkl_sample_ux_set(sample, parameter, NULL)){
        printf("\n>>> There is a problem during the assignment of the parameter Ux.\n");
        exit(EXIT_FAILURE);
    }

    parameter = hkl_sample_uy_get(sample);
    if(TRUE != hkl_parameter_value_set(parameter, uy, HKL_UNIT_USER, NULL) ||
       TRUE != hkl_sample_uy_set(sample, parameter, NULL)){
        printf("\n>>> There is a problem during the assignment of the parameter Uy.\n");
        exit(EXIT_FAILURE);
    }

    parameter = hkl_sample_uz_get(sample);
    if(TRUE != hkl_parameter_value_set(parameter, uz, HKL_UNIT_USER, NULL) ||
       TRUE != hkl_sample_uz_set(sample, parameter, NULL)){
        printf("\n>>> There is a problem during the assignment of the parameter Uz.\n");
        exit(EXIT_FAILURE);
    }
    */

    hkl_geometry_free(geometry);
    hkl_detector_free(detector);

    return sample;
}

HklEngine *new_hkl_engine(char *diffr_type,
                          HklGeometry *geometry, HklDetector *detector, HklSample *sample)
{
    const HklFactory *factory;
    HklEngineList *engines;
    HklEngine *engine;

    factory = hkl_factory_get_by_name(diffr_type, NULL);
    engines = hkl_factory_create_new_engine_list(factory);
    hkl_engine_list_init(engines, geometry, detector, sample);
    engine = hkl_engine_list_engine_get_by_name(engines, engine_type, NULL);

    if(TRUE != hkl_engine_current_mode_set(engine, engine_mode, NULL)){
	    printf("\n>>> There is a problem during the assignment of the engine mode.\n");
        exit(EXIT_FAILURE);
    }

    return engine;
}

void hklmatrix_to_gslmatrix(const HklMatrix *m_hkl, gsl_matrix *m_gsl)
{
    int i, j;
    double x;

    for(i = 0; i < 3; ++i){
        for(j = 0; j < 3; ++j){
            x = hkl_matrix_get(m_hkl, i, j);
            gsl_matrix_set(m_gsl, i, j, x);
        }
    }
}

void cross_product(gsl_vector *u, gsl_vector *v, gsl_vector *vect_prod)
{
    double p1 = gsl_vector_get(u,1)*gsl_vector_get(v,2)
            - gsl_vector_get(u,2)*gsl_vector_get(v,1);

    double p2 = - gsl_vector_get(u,0)*gsl_vector_get(v,2)
            + gsl_vector_get(u,2)*gsl_vector_get(v,0);

    double p3 = gsl_vector_get(u,0)*gsl_vector_get(v,1)
            - gsl_vector_get(u,1)*gsl_vector_get(v,0);

    gsl_vector_set(vect_prod, 0, p1);
    gsl_vector_set(vect_prod, 1, p2);
    gsl_vector_set(vect_prod, 2, p3);
}

void calculate_M_matrix(const HklGeometry *geometry, HklDetector *detector, gsl_matrix *M)
{
    HklQuaternion P_quat;
    HklMatrix *P_hkl;

    gsl_vector *ki_lab, *kf_lab;
    gsl_vector *u1_lab, *u2_lab, *u3_lab;
    gsl_matrix *P, *Minv;

    double vect_norm;
    int i;

    /* ki and kf in the laboratory frame */
    P_quat = hkl_geometry_detector_rotation_get(geometry, detector);
    P_hkl = hkl_matrix_new();
    hkl_quaternion_to_matrix(&P_quat, P_hkl);
    P = gsl_matrix_alloc(3, 3);
    hklmatrix_to_gslmatrix(P_hkl, P);

    ki_lab = gsl_vector_alloc(3);
    gsl_vector_set_zero(ki_lab);
    gsl_vector_set(ki_lab, 0, 1.0);
    kf_lab = gsl_vector_alloc(3);
    gsl_blas_dgemv(CblasNoTrans, 1.0, P, ki_lab, 0.0, kf_lab);

    /* u1, u2, and u3 (basis vectors of the BG reference frame) */
    u1_lab = gsl_vector_alloc(3);
    gsl_vector_memcpy(u1_lab, ki_lab);
    gsl_vector_add(u1_lab, kf_lab);
    vect_norm = gsl_blas_dnrm2(u1_lab);
    gsl_blas_dscal(1.0/vect_norm, u1_lab);

    u2_lab = gsl_vector_alloc(3);
    cross_product(ki_lab, kf_lab, u2_lab);
    vect_norm = gsl_blas_dnrm2(u2_lab);
    gsl_blas_dscal(1.0/vect_norm, u2_lab);

    u3_lab = gsl_vector_alloc(3);
    gsl_vector_memcpy(u3_lab, ki_lab);
    gsl_vector_sub(u3_lab, kf_lab);
    vect_norm = gsl_blas_dnrm2(u3_lab);
    gsl_blas_dscal(1.0/vect_norm, u3_lab);

    /* Matrix transformation from lab to BG reference frame */
    Minv = gsl_matrix_alloc(3, 3);
    for (i = 0; i < 3; ++i){
        gsl_matrix_set(Minv, i, 0, gsl_vector_get(u1_lab, i));
        gsl_matrix_set(Minv, i, 1, gsl_vector_get(u2_lab, i));
        gsl_matrix_set(Minv, i, 2, gsl_vector_get(u3_lab, i));
    }

    /* By construction, the matrix M is orthogonal: inverse(M) = transpose(M) */
    gsl_matrix_memcpy(M, Minv);
    gsl_matrix_transpose(M);
}


void compute_transf_matrix(double *refl_hkl, double trans_matrix[3][3])
{
    HklSample *sample;
    HklGeometry *geometry;
    const HklGeometry *geom;
    HklDetector *detector;
    HklEngine *engine;
    HklGeometryList *geometries;

    HklQuaternion R_quat;
    HklMatrix *R_hkl;
    const HklMatrix *UB_hkl;
    gsl_matrix *UB, *M, *R, *T, *MRUB;

    int i, j;
    double angles[6];
    int n_angles = 6;

    sample = new_hkl_sample();
    geometry = new_hkl_geometry(diffr_type, wavelength);
    detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
    engine = new_hkl_engine(diffr_type, geometry, detector, sample);
    geometries = hkl_engine_pseudo_axis_values_set(engine, refl_hkl, 3, HKL_UNIT_USER, NULL);
    geom = hkl_geometry_list_item_geometry_get(hkl_geometry_list_items_first_get(geometries));

    /* The values of the angles (4S + 2D) can be obtained with */
    hkl_geometry_axis_values_get(geom, angles, n_angles, HKL_UNIT_USER);
    for (i = 0; i < n_angles; ++i){
        printf(" %8.5f ", angles[i]);
    }
    printf("\n");


    /* Matrix transformation from lab to BG reference frame */
    M = gsl_matrix_alloc(3, 3);
    calculate_M_matrix(geom, detector, M);

    /* Matrix transformation from recipr to BG reference frame */
    R_quat = hkl_geometry_sample_rotation_get(geom, sample);
    R_hkl = hkl_matrix_new();
    hkl_quaternion_to_matrix(&R_quat, R_hkl);
    R = gsl_matrix_alloc(3, 3);
    hklmatrix_to_gslmatrix(R_hkl, R);
    UB_hkl = hkl_sample_UB_get(sample);
    UB = gsl_matrix_alloc(3, 3);
    hklmatrix_to_gslmatrix(UB_hkl, UB);
    T = gsl_matrix_alloc(3, 3);
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, R, UB, 0.0, T);
    MRUB = gsl_matrix_alloc(3, 3);
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, M, T, 0.0, MRUB);

    for(i = 0; i < 3; ++i){
        for(j = 0; j < 3; ++j){
            trans_matrix[i][j] = gsl_matrix_get(MRUB, i, j);
        }
    }

    hkl_sample_free(sample);
    hkl_detector_free(detector);
    hkl_geometry_free(geometry);
}

int main(void)
{
    int i, j, k;
    double refl_hkl[] = {0.5, 6.5, 0.43};
    double MRUB_matrix[3][3];

    for(i = 0; i < n_refl; ++i){
        refl_hkl[1] = i + 6.5;
        printf("\nReflection %5.3f, %5.3f, %5.3f :\n", refl_hkl[0], refl_hkl[1], refl_hkl[2]);

        compute_transf_matrix(refl_hkl, MRUB_matrix);
        printf("The transformation matrix:\n");
        for(j = 0; j < 3; ++j){
            for(k = 0; k < 3; ++k){
                printf(" %10.6f ", MRUB_matrix[j][k]);
            }
            printf("\n");
        }
        printf("\n");
    }

    return 0;
}
