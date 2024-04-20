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

---

<iframe src="{{ map_html_path }}" frameborder="0" scrolling="yes" seamless="seamless" style="display:block; width:100%; height:50vh; background: rgba(0,0,0,0);" class="tester"></iframe>

<br>

<iframe src="{{ datatable_html_path }}" onload='javascript:(function(o){o.style.height=o.contentWindow.document.body.scrollHeight+"px";}(this));' style="height:200px;width:100%;border:none;overflow:hidden;padding:0;"></iframe>

{#
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
| Contributor plot ID                                           | {{ contributor_plot_id }}   | -->
#}

Reduce line spacing in table and make narrower

[Coming soon: Download plot data](#)



<script type="application/javascript">
    var iframe = document.getElementById("myIframe");
 
    iframe.onload = function(){
    iframe.contentWindow.document.body.scrollHeight + 'px';
    }
</script>