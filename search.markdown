---
title: Search
---
<script>
  function searchb() {
    document.x.q.value += ' site:dannysu.com';
    return true;
  }
</script>
<form name="x" action="https://duckduckgo.com" onsubmit="return searchb()" target="_top">
  <input type="text" id="q" value="" name="q" size="20" class="searchbox">
</form>
