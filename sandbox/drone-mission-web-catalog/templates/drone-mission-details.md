---
title: Mission {* dataset_id *}

date:
show_date: false
profile: false
---

<div class="container">
    <div class="row">
        <div class="col-sm">
            <div class="text-center">
            {{< cta cta_text="Previous mission" cta_link="{* previous_dataset_page_path *}" cta_new_tab="false" >}}
            </div>
        </div>
        <div class="col-sm">
            <div class="text-center">
            {{< cta cta_text="Next mission" cta_link="{* next_dataset_page_path *}" cta_new_tab="false" >}}
            </div>
        </div>
    </div>
</div>

<p style="line-height: 125%; text-align:center;"><b>This page is in beta.</b> The drone data web catalog is under active development and will continue to improve. Feel free to <a href="/about/#contact-us">contact us</a> with feedback!</p>

{% if oblique -%}
<p style="color: red; line-height: 125%; text-align:center;">Note: This dataset was collected using an oblique camera pitch and is intended to complement a co-located mission that was flown using a nadir camera. Any processed data products on this page were produced from the oblique dataset only and are not expected to be of high quality.</p>

{% endif -%}


{% if ttops_exists -%}

## Detected trees

<iframe src="{* itd_map_html_path *}" frameborder="0" scrolling="yes" seamless="seamless" style="display:block; width:100%; height:75vh; background: rgba(0,0,0,0);" class="tester"></iframe>

[Download tree points]({* ttops_url *})
{% endif -%}

{% if ortho_exists -%}

## Orthomosaic

{{< figure src="{* ortho_url_thumb *}" caption="[Download full orthomosaic]({* ortho_url_full *})" >}}
{% endif -%}

{% if chm_exists -%}

## Canopy height model

{{< figure src="{* chm_url_thumb *}" caption="[Download full CHM]({* chm_url_full *})" >}}

{% endif -%}

{% if dsm_exists -%}

## Digital surface model

{{< figure src="{* dsm_url_thumb *}" caption="[Download full DSM]({* dsm_url_full *})" >}}

{% endif -%}

{% if dtm_exists -%}

## Digital terrain model

{{< figure src="{* dtm_url_thumb *}" caption="[Download full DTM]({* dtm_url_full *})" >}}

<br>

{% endif -%}

{% if pc_exists -%}

## Point cloud

Preview in development. For now, you can paste [this url]({* pc_url_full *}) into a point cloud viewer like [Eptium](https://viewer.copc.io/).

[Download full point cloud]({* pc_url_full *})

<br>

{% endif -%}

{% if mesh_exists -%}

## Mesh model

Preview in development.

[Download full mesh model]({* mesh_url_full *})

<br>

{% endif -%}

{% if images_example_exists or images_zip_exists -%}

## Raw drone images

{% if images_example_exists -%}

#### Example images

<div class="container">
    <div class="row">
        <div class="col-sm">
            {{< figure src="{* images_example_url_thumb *}example_1.JPG" link="{* images_example_url_full *}example_1.JPG" width="90%">}}
        </div>
        <div class="col-sm">
            {{< figure src="{* images_example_url_thumb *}example_2.JPG" link="{* images_example_url_full *}example_2.JPG" width="90%">}}
        </div>
    </div>
    <div class="row">
        <div class="col-sm">
            {{< figure src="{* images_example_url_thumb *}example_3.JPG" link="{* images_example_url_full *}example_3.JPG" width="90%">}}
        </div>
        <div class="col-sm">
            {{< figure src="{* images_example_url_thumb *}example_4.JPG" link="{* images_example_url_full *}example_4.JPG" width="90%">}}
        </div>
    </div>
</div>

{% endif -%}

{% if images_zip_exists -%}

#### Full image set

[Download full image
set]({* images_zip_url *})

{% endif -%}

<br>

{% endif -%}

## Image locations and attributes

<iframe src="{* map_html_path *}" frameborder="0" scrolling="yes" seamless="seamless" style="display:block; width:100%; height:75vh; background: rgba(0,0,0,0);" class="tester"></iframe>

<br>

<iframe src="{* datatable_html_path *}" onload='javascript:(function(o){o.style.height=o.contentWindow.document.body.scrollHeight+"px";}(this));' style="height:200px;width:100%;border:none;overflow:hidden;padding:0;"></iframe>

<br>

{% if footprint_exists or cameras_exists or log_exists -%}

## Other data

{% if footprint_exists -%}
[Mission geospatial footprint]({* footprint_url *})

{% endif -%}

{% if cameras_exists -%}
[Photogrammetry camera locations and parameters]( {* cameras_url *})

{% endif -%}

{% if log_exists -%}
[Photogrammetry processing log]({* log_url *})

{% endif -%}

{% endif -%}


<!-- Script to make the datatable the height to fit the data -->
<script type="application/javascript">
    var iframe = document.getElementById("myIframe");
 
    iframe.onload = function(){
    iframe.contentWindow.document.body.scrollHeight + 'px';
    }
</script>
