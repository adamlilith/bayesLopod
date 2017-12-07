#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4psiip_mod) {


    class_<rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> >("model_psiip")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_psiip_namespace::model_psiip, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4psiip_CAR_mod) {


    class_<rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> >("model_psiip_CAR")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_psiip_CAR_namespace::model_psiip_CAR, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4psiipi_mod) {


    class_<rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> >("model_psiipi")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_psiipi_namespace::model_psiipi, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4psiipi_CAR_mod) {


    class_<rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> >("model_psiipi_CAR")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_psiipi_CAR_namespace::model_psiipi_CAR, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4psiipiq_mod) {


    class_<rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> >("model_psiipiq")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_psiipiq_namespace::model_psiipiq, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4psiipiq_CAR_mod) {


    class_<rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> >("model_psiipiq_CAR")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_psiipiq_CAR_namespace::model_psiipiq_CAR, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4psiipq_mod) {


    class_<rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> >("model_psiipq")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_psiipq_namespace::model_psiipq, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4psiipq_CAR_mod) {


    class_<rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> >("model_psiipq_CAR")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_psiipq_CAR_namespace::model_psiipq_CAR, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
