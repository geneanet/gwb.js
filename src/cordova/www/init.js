function initFS () {

    var w = new Worker('gwb.bc.js') ;
    var data = [] ;
    var input = document.getElementById ("input");

    window.requestFileSystem  = window.requestFileSystem || window.webkitRequestFileSystem ;

    window.requestFileSystem(window.TEMPORARY, 1024*1024, function(fs) {
        fs.root.getDirectory('grimaldi.gwb', {create: true}, function(dirEntry) { console.log ("Created directory grimaldi") ; } );
        fs.root.getDirectory('cnt', {create: true}, function(dirEntry) { console.log ("Created directory cnt") ; } );
    });


    input.onchange = function () {
        var files = input.files ;
        for (var i = 0; i < files.length; i++) {
            (function (i) {
                var file = files[i] ;
                console.log ("Reading " + file.name) ;
                var fr = new FileReader () ;
                fr.onload = function () { data.push({ name: file.name, data: fr.result }) ; }
                fr.readAsBinaryString (file) ;
            }) (i) ;
        }
        var loop = function () {
            if (data.length == files.length) {
                console.log ('Sending') ;
                console.log (data) ;
                w.postMessage ({type: 'loadFiles', data:data}) ;
                w.postMessage ({type: 'openBase'})
            }
            else { console.log ('loop ' + data.length + '/' + files.length) ; setTimeout (loop, 500) ; }
        } ;
        loop () ;
    }

    Page = {} ;

    Page.person = function (p) {
        w.postMessage ({ type: 'display', data: { page: 'person', payload: p } }) ;
    }
    Page.summary = function () {
        w.postMessage ({ type: 'display', data: { page: 'summary', payload: null } }) ;
    }
    Page.searchPerson = function (fn, sn, occ) {
        w.postMessage ({ type: 'display', data: { page: 'searchPerson', payload: [ fn, sn, occ ] } }) ;
    }
    Page.oldestAlive = function () {
        w.postMessage ({ type: 'display', data: { page: 'oldestAlive', payload: null } }) ;
    }

    w.onmessage = function(e) { document.body.innerHTML = e.data ; console.log('Message received from worker') }
}

