---
title: Mission {{ dataset_id }}

date:
show_date: false
profile: false
---

<p style="color: red; line-height: 125%;">{{ top_message }}</p>

<div class="container">
    <div class="row">
        <div class="col-sm">
            <div class="text-center">
            <!-- Need to escape the Jinjar render, so pass the CTA shortcode as a jinjar string expression, which Jinjar will output into what is read by Hugo -->
            {{ "{{< cta cta_text=\"< Previous mission\" cta_link=\"" }}{{ previous_dataset_page_path }}{{ " \"cta_new_tab=\"false\" >}}" }}
            </div>
        </div>
        <div class="col-sm">
            <div class="text-center">
            {{ "{{< cta cta_text=\" Next mission >\" cta_link=\"" }}{{ next_dataset_page_path }}{{ " \"cta_new_tab=\"false\" >}}" }}
            </div>
        </div>
    </div>
</div>

<iframe src="{{ map_html_path }}" frameborder="0" scrolling="yes" seamless="seamless" style="display:block; width:100%; height:75vh; background: rgba(0,0,0,0);" class="tester"></iframe>

<br>

<iframe src="{{ datatable_html_path }}" onload='javascript:(function(o){o.style.height=o.contentWindow.document.body.scrollHeight+"px";}(this));' style="height:200px;width:100%;border:none;overflow:hidden;padding:0;"></iframe>

[Coming soon: Download dataset](#)

<!-- Script to make the datatable the height to fit the data -->
<script type="application/javascript">
    var iframe = document.getElementById("myIframe");
 
    iframe.onload = function(){
    iframe.contentWindow.document.body.scrollHeight + 'px';
    }
</script>
