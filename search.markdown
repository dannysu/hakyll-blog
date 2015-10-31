---
title: Search
---
<form method="get" id="sform" action="/search.html">
  <input type="text" id="q" value="" name="q" size="20" class="searchbox">
</form>
<div id="cse">Loading</div>
<script type="text/javascript">
  (function() {
    document.getElementById('q').value = decodeURIComponent(window.location.search.substring(3)).replace(/\+/g, ' ');
    var loadingDiv = document.getElementById('cse');
    var count = 0;
    var handle = setInterval(function() {
        var text = loadingDiv.innerHTML.substr(0, 8).trim();
        if (text === 'Loading') {
            if (count >= 3) {
                loadingDiv.innerHTML = 'Loading';
                count = 0;
            }
            else {
                loadingDiv.innerHTML = loadingDiv.innerHTML + ' .';
                count++;
            }
        }
        else {
            clearInterval(handle);
        }
    }, 500);
  })();
</script>
<script src="//www.google.com/jsapi" type="text/javascript"></script>
<script type="text/javascript"> 
  google.load('search', '1', {language : 'en', style : google.loader.themes.MINIMALIST});
  google.setOnLoadCallback(function() {
    var customSearchOptions = {};  var customSearchControl = new google.search.CustomSearchControl(
      '014002343956161213633:sxmdlmxxazq', customSearchOptions);
    customSearchControl.setResultSetSize(google.search.Search.FILTERED_CSE_RESULTSET);
    var options = new google.search.DrawOptions();
    options.enableSearchResultsOnly(); 
    customSearchControl.draw('cse', options);
    function parseParamsFromUrl() {
      var params = {};
      var parts = window.location.search.substr(1).split('\x26');
      for (var i = 0; i < parts.length; i++) {
        var keyValuePair = parts[i].split('=');
        var key = decodeURIComponent(keyValuePair[0]);
        params[key] = keyValuePair[1] ?
            decodeURIComponent(keyValuePair[1].replace(/\+/g, ' ')) :
            keyValuePair[1];
      }
      return params;
    }

    var urlParams = parseParamsFromUrl();
    var queryParamName = "q";
    if (urlParams[queryParamName]) {
      customSearchControl.execute(urlParams[queryParamName]);
    }
  }, true);
</script>
