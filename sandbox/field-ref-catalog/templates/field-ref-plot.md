---
title: Plot {{ plot_id }}

date: 2022-01-01
show_date: false
profile: false
image:
  focal_point: ''
  preview_only: true
  # caption: Automatically detected treetops, with point size indicating tree height, overlaid on drone-derived orthoimagery from the Tahoe National Forest

# banner:
#   caption:
#   image: "tahoe2.jpg"

another: {{ another_param }}
---


<<<< Map goes here >>>>


| Plot attributes                                               |                             |
| ------------------------------------------------------------- | --------------------------- |
| Plot ID                                                       | {{ plot_id }}               |
| Project ID                                                    | {{ project_id }}            |
| Measurement year                                              | {{ survey_year }}           |
| Plot area (ha)                                                | {{ plot_area_ha }}          |
| Tree count                                                    | {{ n_trees }}               |
| Basal area (m2/ha)                                            | {{ ba_ha }}                 |
| Top species                                                   | {{ top_species }}           |
| Height measured                                               | {{ height_measured }}       |
| Minimum DBH measured (cm)                                     | {{ min_dbh }}               |
| Minimum height measured (for trees visible from overhead) (m) | {{ min_ht_ohvis }}          |
| Minimum height measured (for other trees) (m)                 | {{ min_ht }}                |
| Data license                                                  | {{ license_short }}         |
| Data license details                                          | {{ license }}               |
| Contributor/Investigator                                      | {{ investigator_names }} |
| Contributor plot ID                                           | {{ contributor_plot_id }}   |

Reduce line spacing in table and make narrower

[Coming soon: Download plot data](#)