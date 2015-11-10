(function() {
    'use strict';

    var primary = document.getElementById('primary');
    var images = primary.getElementsByTagName('img');
    var i, image;

    for (i = 0; i < images.length; i++) {
        image = images[i];

        if (image.parentElement.tagName.toUpperCase() === 'A') {
            image.parentElement.target = '_blank';
        }
    }

    document.getElementById('email').href = 'mailto:contact@dannysu.com';
})();
