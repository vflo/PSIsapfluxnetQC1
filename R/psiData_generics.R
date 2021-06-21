#' psiData custom get generics
#'
#' Generics for getting the info in the slots of psiData
#'
#' @name psi_get_generics
#' @include psiData_class.R
NULL

#' @rdname psi_get_generics
#' @export
setGeneric(
  "get_psi",
  function(object, ...) {
    standardGeneric("get_psi")
  }
)


#' @rdname psi_get_generics
#' @export
setGeneric(
  "get_psi_flags",
  function(object, ...) {
    standardGeneric("get_psi_flags")
  }
)


#' @rdname psi_get_generics
#' @export
setGeneric(
  "get_timestamp",
  function(object, ...) {
    standardGeneric("get_timestamp")
  }
)

#' @rdname psi_get_generics
#' @export
setGeneric(
  "get_solar_timestamp",
  function(object, ...) {
    standardGeneric("get_solar_timestamp")
  }
)

#' @rdname psi_get_generics
#' @export
setGeneric(
  "get_si_code",
  function(object, ...) {
    standardGeneric("get_si_code")
  }
)

#' @rdname psi_get_generics
#' @export
setGeneric(
  "get_site_md",
  function(object, ...) {
    standardGeneric("get_site_md")
  }
)


#' @rdname psi_get_generics
#' @export
setGeneric(
  "get_plant_md",
  function(object, ...) {
    standardGeneric("get_plant_md")
  }
)

#' @rdname psi_get_generics
#' @export
setGeneric(
  "get_question_md",
  function(object, ...) {
    standardGeneric("get_question_md")
  }
)

#' Replacement generics
#'
#' Generic functions for replacement functions for psiData
#'
#' @name psi_replacement_generics
NULL

#' @rdname psi_replacement_generics
#' @export
setGeneric(
  "get_psi<-",
  function(object, value) {
    standardGeneric("get_psi<-")
  }
)


#' @rdname psi_replacement_generics
#' @export
setGeneric(
  "get_psi_flags<-",
  function(object, value) {
    standardGeneric("get_psi_flags<-")
  }
)


#' @rdname psi_replacement_generics
#' @export
setGeneric(
  "get_timestamp<-",
  function(object, value) {
    standardGeneric("get_timestamp<-")
  }
)

#' @rdname psi_replacement_generics
#' @export
setGeneric(
  "get_solar_timestamp<-",
  function(object, value) {
    standardGeneric("get_solar_timestamp<-")
  }
)

#' @rdname psi_replacement_generics
#' @export
setGeneric(
  "get_si_code<-",
  function(object, value) {
    standardGeneric("get_si_code<-")
  }
)

#' @rdname psi_replacement_generics
#' @export
setGeneric(
  "get_site_md<-",
  function(object, value) {
    standardGeneric("get_site_md<-")
  }
)


#' @rdname psi_replacement_generics
#' @export
setGeneric(
  "get_plant_md<-",
  function(object, value) {
    standardGeneric("get_plant_md<-")
  }
)

#' @rdname psi_replacement_generics
#' @export
setGeneric(
  "get_question_md<-",
  function(object, value) {
    standardGeneric("get_question_md<-")
  }
)
