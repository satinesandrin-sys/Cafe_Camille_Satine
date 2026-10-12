// Agrandir un graphique plotly et le télécharger en PNG sur fond blanc.
(function () {
  var TAILLE_PNG = { width: 1600, height: 1000, scale: 2 };   // 3200 x 2000 pixels au final

  // Téléchargement PNG sur fond blanc : on passe temporairement les fonds en blanc
  // (ils sont transparents à l'écran), on exporte, puis on remet les fonds d'origine.
  function telecharger(gd, nom) {
    var p0 = (gd.layout && gd.layout.paper_bgcolor) || 'rgba(0,0,0,0)';
    var q0 = (gd.layout && gd.layout.plot_bgcolor) || 'rgba(0,0,0,0)';
    var restaurer = function () { return Plotly.relayout(gd, { paper_bgcolor: p0, plot_bgcolor: q0 }); };
    return Plotly.relayout(gd, { paper_bgcolor: '#ffffff', plot_bgcolor: '#ffffff' })
      .then(function () {
        return Plotly.downloadImage(gd, { format: 'png', width: TAILLE_PNG.width,
                                          height: TAILLE_PNG.height, scale: TAILLE_PNG.scale, filename: nom });
      })
      .then(restaurer, function (e) { console.error(e); return restaurer(); });
  }

  // Fenêtre plein écran contenant une copie du graphique, avec ses propres boutons.
  function agrandir(gd) {
    var data = JSON.parse(JSON.stringify(gd.data));
    var layout = JSON.parse(JSON.stringify(gd.layout));
    delete layout.width; delete layout.height;                   // la taille suit la fenêtre
    layout.autosize = true; layout.paper_bgcolor = '#ffffff'; layout.plot_bgcolor = '#ffffff';
    var nom = 'cafe_' + (gd.id || 'graphique');

    var ov = document.createElement('div');
    ov.className = 'cafe-overlay';
    ov.innerHTML = '<div class="cafe-fenetre"><div class="cafe-entete">' +
      '<button class="cafe-btn" data-act="png">Télécharger en PNG</button>' +
      '<button class="cafe-btn" data-act="fermer">Fermer</button></div>' +
      '<div class="cafe-zone"></div></div>';
    document.body.appendChild(ov);
    var zone = ov.querySelector('.cafe-zone');

    function fermer() {
      document.removeEventListener('keydown', surTouche);
      try { Plotly.purge(zone); } catch (e) {}
      ov.remove();
    }
    function surTouche(e) { if (e.key === 'Escape') fermer(); }
    document.addEventListener('keydown', surTouche);
    ov.addEventListener('click', function (e) { if (e.target === ov) fermer(); });   // clic à côté = fermer
    ov.querySelector('[data-act="fermer"]').onclick = fermer;
    ov.querySelector('[data-act="png"]').onclick = function () { telecharger(zone, nom); };

    Plotly.newPlot(zone, data, layout, { responsive: true, displaylogo: false, displayModeBar: false });
  }

  // Ajoute les deux boutons en haut à gauche de chaque graphique plotly de la page.
  function ajouterOutils() {
    document.querySelectorAll('.js-plotly-plot').forEach(function (gd) {
      if (!gd.data || gd.closest('.cafe-overlay') || gd.querySelector(':scope > .cafe-outils')) return;
      var barre = document.createElement('div');
      barre.className = 'cafe-outils';
      barre.innerHTML = '<button class="cafe-btn" data-act="agrandir" title="Agrandir">Agrandir</button>' +
                        '<button class="cafe-btn" data-act="png" title="Télécharger en PNG (fond blanc)">PNG</button>';
      barre.addEventListener('click', function (e) {
        var b = e.target.closest('button'); if (!b) return;
        e.stopPropagation();
        if (b.getAttribute('data-act') === 'agrandir') agrandir(gd);
        else telecharger(gd, 'cafe_' + (gd.id || 'graphique'));
      });
      gd.appendChild(barre);
    });
  }

  // Shiny recrée les graphiques à chaque changement : on surveille la page et on remet les boutons.
  var planifie = null;
  new MutationObserver(function () {
    if (planifie) return;
    planifie = setTimeout(function () { planifie = null; ajouterOutils(); }, 250);
  }).observe(document.body, { childList: true, subtree: true });
})();