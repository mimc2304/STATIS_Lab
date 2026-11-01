// Resalta en la cinta el análisis activo (estilo jamovi).
$(document).on("shiny:connected", function () {
  Shiny.addCustomMessageHandler("bl-activo", function (id) {
    $(".bl-ribbon-btn").removeClass("bl-activo");
    $(id === "datos" ? "#ir_datos" : "#abrir_" + id).addClass("bl-activo");
  });
  $("#ir_datos").addClass("bl-activo");
  Shiny.setInputValue("seccion_cinta", "Biplot");
  // Pestañas de la cinta: Biplot o K-tablas
  $(".bl-ribbon-tab").on("click", function () {
    var grupo = $(this).data("grupo");
    Shiny.setInputValue("seccion_cinta", grupo);
    $(".bl-ribbon-tab").removeClass("bl-activa");
    $(this).addClass("bl-activa");
    $(".bl-ribbon-menus .bl-ribbon-group").each(function () {
      $(this).toggleClass("bl-oculto", $(this).data("grupo") !== grupo);
    });
  });
});
