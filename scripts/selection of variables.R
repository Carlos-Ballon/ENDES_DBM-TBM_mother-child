variable_selection_endes <- function(data) {
  data |>
  dplyr::select(
    # Diseño de la muestra
    HHID,  # Identificador *
    HV001, # Conglomerado *
    HV022, # Estrato *
    HV005, # Factor ponderación hogar *
    V005,  # Factor ponderación mujer *

    # Sociodemograficas del hogar
    HV025, # Area de residencia *
    HV040, # Altura *
    HV023, # Departamento *
    SHREGION, # Región natural *
    HV270, # Indice de riqueza *
    HV271, # Puntuación indice de riqueza *
    
    # Madre 15-49 años
    HV101, # Relación de parentesco con el jefe del hogar *
    HV104, # Sexo de la madre *
    HA1,   # Edad de la madre 12:49 años *
    HV105, # Edad de la madre 1:98 *
    HV115, # Estado civil *
    QH25A, # Nacionalidad *
    HA65,  # Resultado de entrevista individual *
    HV109, # Nivel educativo de la madre * 
    HA66,  # Maximo nivel educativo de la madre *
    HA2,   # Peso (Kg) *
    HA3,   # Talla (cm) *
    HA40,  # IMC * 
    HA41,  # Indice de Rohrer *
    HA53,  # Hemoglobina (g/dL-1 decimal), *
    HA55,  # Resultado de medición de hemoglobina *
    HA56,  # Hemoglobina ajustada por altitud (g/dL-1 decimal) *
    HA57,  # Nivel de anemia de la madre *
    HA54,  # Actualmente embarazada *
    V212,  # Edad del entrevistado al primer nacimiento *
    M45,   # Durante el embarazo, le administraron tabletas, jarabe o inyecciones de hierro *
    M46,   # Por cuantos días tomó hierro y/o cuantas inyecciones recibió *
    M54,   # Recibió una dosis de vitamina A en los primeros 2 meses después del parto *
    M14,   # Visitas prenatales por embarazo *
    M66,   # Después del parto tuvo algún chequeo o revisiòn médica
    M67,   # Cuánto tiempo después del parto se realizó su primer chequeo o revisiòn médica
    M71,   # Cuánto tiempo después del parto se realizó el control postnatal *
    V201,  # Total de niños nacidos *
    V208,  # Nacimientos en los últimos cinco años *
    M17,   # Parto por cesárea *
    QS26,  # Seguro de salud *
    M4,    # Duración de la lactancia *
    M5,    # Meses de amamantamiento *
    V404,  # Actualmente amamantando *
    
    # Niño 
    HC1,   # Edad en meses *
    HC2,   # Peso (Kg) *
    HC27,  # Sexo del niño *
    M18,   # Tamaño del niño al nacer *
    M19,   # Peso del niño al nacer (kilos - 3 dec.) *
    MIDX,  # Orden de nacimiento *
    HA4,   # Talla/Edad percentil
    HA5,   # Talla/Edad desviacion estandar
    HA6,   # Talla/Edad porcentaje respecto a la mediana
    HA11,  # Peso/Talla desviacion estándar (DHS)
    HA12,  # Peso/Talla Porcentaje respecto a la mediana (DHS)
    HA12A, # Peso/Talla Porcentaje respecto a la mediana (Foggarty)
    HA12B, # Peso/Talla Porcentaje respecto a la mediana (OMS)
    HC70,  # Talla/Edad de la Desviación Estándar de la mediana de referencia (según la OMS) *
    HC71,  # Peso/Edad de la Desviación Estándar de la mediana de referencia (según la OMS) *
    HC72,  # Peso/Talla Desviación Estándar de la mediana de referencia (según la OMS) *
    HC73,  # Desviación Estándar del IMC (según la OMS) *
    HC53,  # Hemoglobina del niño (g/dL-1 decimal) *
    HC55,  # Resultado de medición de hemoglobina en niños *
    HC56,  # Hemoglobina ajustada por altitud en niños (g/dL-1 decimal) *
    HC57,  # Nivel de anemia del niño *
    
    # Casa
    HV219, # Sexo del jefe de hogar *
    HV220, # Edad del jefe de hogar *
    HV014, # Número de niños menores de 5 años *
    HV035, # Número de niños elegibles para altura y peso
    HV103,  # ¿Durmió aquí anoche?
    HV201,  # ¿Cuál es la fuente principal de abastecimiento de agua para tomar o beber?
    HV205,  # ¿Qué tipo de servicio higiénico tiene su hogar?
    HV206,  # ¿Tiene electricidad en su hogar?
    HV214,  # Material predominante del piso de la vivienda
    HV226,  # ¿Cuál es el combustible que utilizan más frecuentemente en su hogar para cocinar? 
    HC60    # Número de orden de la madre en el hogar
  ) |>
  dplyr::rename(
    # Diseño de la muestra
    id = HHID,
    psu = HV001,
    stratum = HV022,
    sample_weight_house = HV005,
    sample_weight_women = V005,
    
    # Sociodemograficas del hogar
    urb_rur = HV025,
    altitude = HV040,
    department = HV023,
    region_natural = SHREGION,
    wealth_index = HV270, 
    wealth_score = HV271,
    
    # Madre 15-49 años
    parentesco_jefe = HV101,
    sex_mother = HV104,
    age_mother = HV105,
    age_mother_49 = HA1,
    estado_civil = HV115,
    nacionalidad = QH25A,
    entrevista_mother = HA65,
    education_mother = HV109,
    max_education_mother = HA66,
    height_mother = HA3,
    weight_mother = HA2,
    bmi_mother = HA40,
    idx_rohrer = HA41,
    hb_mother = HA53,
    entrevista_anemia_mother = HA55,
    hb_altura_mother = HA56,
    level_anemia_mother = HA57,
    currently_pregnant = HA54,
    age_first_birth = V212,
    iron_in_pregnancy = M45,
    days_iron_in_pregnancy = M46,
    two_month_vitamin_a = M54,
    atencion_prenatal = M14,
    atencion_postnatal = M71,
    total_nacidos = V201,
    birth_last5years = V208,
    cesarean_birth = M17,
    health_insurance = QS26,
    current_breastfeeding = V404,
    breastfeeding_duration = M4,
    months_breastfeeding = M5,
    
    # Niño
    age_child = HC1,
    sex_child = HC27,
    weight_child = HC2,
    birth_size = M18,
    birth_weight = M19,
    sd_height_age_child = HC70,
    sd_weight_age_child = HC71,
    sd_weight_height_child = HC72,
    sd_bmi_child = HC73,
    hb_child = HC53, 
    entrevista_anemia_child = HA55,
    hb_altura_child = HC56,
    level_anemia_child = HC57,
    orden_nacimiento = MIDX,
    
    # Casa
    sexo_jefe = HV219,
    edad_jefe = HV220,
    child_number5years = HV014,
    durmio_noche = HV103,
    water = HV201,
    drain = HV205,
    electricity = HV206,
    material_casa = HV214,
    combustible_cocina = HV226,
    numero_madre = HC60
  )
}

