---
title: Plot {{ plot_id }}

date:
show_date: false
profile: false
---

<p style="color: red; line-height: 125%;">{{ top_message }}</p>

<iframe src="{{ map_html_path }}" frameborder="0" scrolling="yes" seamless="seamless" style="display:block; width:100%; height:50vh; background: rgba(0,0,0,0);" class="tester"></iframe>

<br>

<iframe src="{{ datatable_html_path }}" onload='javascript:(function(o){o.style.height=o.contentWindow.document.body.scrollHeight+"px";}(this));' style="height:200px;width:100%;border:none;overflow:hidden;padding:0;"></iframe>

[Coming soon: Download plot data](#)


<!-- Script to make the datatable the height to fit the data -->
<script type="application/javascript">
    var iframe = document.getElementById("myIframe");
 
    iframe.onload = function(){
    iframe.contentWindow.document.body.scrollHeight + 'px';
    }
</script>
