# EconSur · IPC Argentina 🇦🇷

Dashboard interactivo con la serie histórica del **Índice de Precios al Consumidor (IPC)** publicado por el INDEC, desagregado por todas las divisiones COICOP a nivel Nacional.

## Stack

| Capa | Tecnología |
|------|-----------|
| Lenguaje | R 4.3 |
| UI framework | bs4Dash + Shiny |
| Gráficos | Highcharter |
| Deploy | Render (Docker) |
| Datos | INDEC – `serie_ipc_divisiones.csv` |

## Estructura del repositorio

```
econsur-inflacion/
├── app.R                  # App principal (UI + Server + ETL)
├── data/
│   └── serie_ipc_divisiones.csv   # Serie IPC INDEC
├── Dockerfile             # Imagen Docker para Render
├── shiny-server.conf      # Config Shiny Server (PORT dinámico)
├── render.yaml            # Blueprint de Render
└── .gitignore
```

## Vistas del dashboard

| Tab | Descripción |
|-----|------------|
| **Principal** | KPIs + Serie interanual rodante + Acumulado anual por sector |
| **Por Sector** | Gráfico de barras mensual para un rubro seleccionado |
| **Comparar Rubros** | Barras comparativas de todos los rubros (mensual o interanual) |
| **Acerca de** | Info del proyecto y fuentes |

## Deploy en Render

### Pasos

1. **Crear repo en GitHub** y subir todos los archivos.

2. En [render.com](https://render.com) hacer click en **New → Web Service**.

3. Conectar el repo de GitHub.

4. Render detecta el `render.yaml` automáticamente (o configurar manualmente):
   - **Runtime**: Docker
   - **Dockerfile path**: `./Dockerfile`
   - **Port**: `3838`

5. Click en **Create Web Service** → Render construye la imagen y despliega.

> El primer build tarda ~5-10 min porque instala los paquetes de R.  
> Los builds siguientes son más rápidos gracias al cache de capas Docker.

### Variables de entorno

Render inyecta `PORT` automáticamente.  
No se necesitan otras variables de entorno para el funcionamiento básico.

## Datos

El archivo `data/serie_ipc_divisiones.csv` contiene:

| Campo | Descripción |
|-------|------------|
| `Codigo` | Código COICOP |
| `Descripcion` | Nombre del rubro |
| `Clasificador` | Tipo de clasificación |
| `Periodo` | Período (YYYYMM) |
| `Indice_IPC` | Índice base dic-2016=100 |
| `v_m_IPC` | Variación mensual % |
| `v_i_a_IPC` | Variación interanual % |
| `Region` | GBA / Nacional / Pampeana / ... |

Para actualizar los datos: reemplazar `data/serie_ipc_divisiones.csv` y hacer push.  
Render redespliega automáticamente.

## Actualización automática con GitHub Actions (opcional)

Podés agregar un workflow `.github/workflows/update_data.yml` que descargue el CSV actualizado del INDEC y haga push, disparando un nuevo deploy en Render.

---

Fuente de datos: [INDEC Argentina](https://www.indec.gob.ar/)
