// Agrandir un graphique plotly et le télécharger en PNG (fond blanc, avec titre).
(function () {
  var TAILLE_PNG = { width: 1600, height: 1000, scale: 2 };   // 3200 x 2000 pixels au final

  // ---- Petits outils -------------------------------------------------------------
  // txt : le titre plotly peut être un texte ou un objet {text: ...}.
  function txt(t) { return t && typeof t === 'object' ? (t.text || '') : (t || ''); }
  // valeur : valeur actuelle d'un contrôle de la page (liste déroulante, case...).
  function valeur(id) { var el = document.getElementById(id); return el && el.value ? el.value : ''; }
  // axe : titre d'un axe du graphique (ex. « Dimension 1 (28.3 %) »).
  function axe(gd, a) { return gd.layout && gd.layout[a] ? txt(gd.layout[a].title) : ''; }

  // ---- Titre à écrire sur l'image ------------------------------------------------
  // titreExport(gd) : texte du titre selon le graphique (identifié par son id Shiny, gd.id).
  // <br> = retour à la ligne ; <sup>...</sup> = sous-titre en petit.
  function titreExport(gd) {
    var id = gd.id || '';
    var propre = txt(gd.layout && gd.layout.title);               // titre déjà présent dans le graphique
    var plan = (axe(gd, 'xaxis') && axe(gd, 'yaxis')) ? axe(gd, 'xaxis') + ' et ' + axe(gd, 'yaxis') : '';
    if (id === 'radar') {                                         // radar combiné : liste des pays tracés
      var noms = (gd.data || []).map(function (t) { return t.name; }).filter(Boolean);
      return 'Profil sensoriel : ' + noms.join(', ');
    }
    if (/^radar_[0-9]+$/.test(id)) return 'Profil sensoriel : ' + propre;     // radar d'un seul pays
    if (id === 'hist_pays') {                                     // histogramme du pays cliqué sur la carte
      var h = document.querySelector('#detail_pays h4');
      return 'Distribution des scores totaux' + (h ? ' : ' + h.textContent : '');
    }
    if (id === 'af_cercle') return 'AFDM : cercle des corrélations<br><sup>' + plan + '</sup>';
    if (id === 'af_ind')
      return 'AFDM : lots dans le plan factoriel<br><sup>Colorés selon : ' + valeur('af_col') + ' - ' + plan + '</sup>';
    if (/^af_contrib/.test(id)) return 'AFDM : contribution des variables - ' + propre;
    return propre || '';                                          // autre graphique : son titre s'il en a un
  }

  // ---- Téléchargement PNG --------------------------------------------------------
  // Pendant l'export, on passe temporairement les fonds en blanc et on affiche le titre,
  // puis on remet l'état d'origine (à l'écran, les fonds restent transparents, sans titre).
  function telecharger(gd, nom, titre) {
    var lay = gd.layout || {};
    var orig = {
      p: lay.paper_bgcolor || 'rgba(0,0,0,0)',
      q: lay.plot_bgcolor || 'rgba(0,0,0,0)',
      titre: lay.title ? JSON.parse(JSON.stringify(lay.title)) : null,
      mt: lay.margin && lay.margin.t != null ? lay.margin.t : null
    };
    var maj = { paper_bgcolor: '#ffffff', plot_bgcolor: '#ffffff' };
    if (titre) {
      maj['title.text'] = titre;
      maj['title.x'] = 0.5;
      maj['title.xanchor'] = 'center';
      maj['title.font.size'] = 22;
      maj['title.font.color'] = '#2b1a12';
      maj['margin.t'] = Math.max(orig.mt || 0, 100);              // place pour le titre en haut
    }
    var restaurer = function () {
      return Plotly.relayout(gd, {
        paper_bgcolor: orig.p, plot_bgcolor: orig.q,
        title: orig.titre || { text: '' },
        'margin.t': orig.mt != null ? orig.mt : 25
      });
    };
    return Plotly.relayout(gd, maj)
      .then(function () {
        return Plotly.downloadImage(gd, { format: 'png', width: TAILLE_PNG.width,
                                          height: TAILLE_PNG.height, scale: TAILLE_PNG.scale, filename: nom });
      })
      .then(restaurer, function (e) { console.error(e); return restaurer(); });
  }

  // ---- Fenêtre d'agrandissement --------------------------------------------------
  // Copie du graphique dans une fenêtre plein écran, avec son titre et ses propres boutons.
  function agrandir(gd) {
    var titre = titreExport(gd);
    var data = JSON.parse(JSON.stringify(gd.data));
    var layout = JSON.parse(JSON.stringify(gd.layout));
    delete layout.width; delete layout.height;                   // la taille suit la fenêtre
    layout.autosize = true; layout.paper_bgcolor = '#ffffff'; layout.plot_bgcolor = '#ffffff';
    if (titre) {
      layout.title = { text: titre, x: 0.5, xanchor: 'center', font: { size: 20, color: '#2b1a12' } };
      layout.margin = Object.assign({}, layout.margin, { t: 100 });
    }
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
    ov.querySelector('[data-act="png"]').onclick = function () { telecharger(zone, nom, titre); };

    Plotly.newPlot(zone, data, layout, { responsive: true, displaylogo: false, displayModeBar: false });
  }

  // ---- Boutons sur chaque graphique ----------------------------------------------
  function ajouterOutils() {
    document.querySelectorAll('.js-plotly-plot').forEach(function (gd) {
      if (!gd.data || gd.closest('.cafe-overlay') || gd.querySelector(':scope > .cafe-outils')) return;
      var barre = document.createElement('div');
      barre.className = 'cafe-outils';
      barre.innerHTML = '<button class="cafe-btn" data-act="agrandir" title="Agrandir">Agrandir</button>' +
                        '<button class="cafe-btn" data-act="png" title="Télécharger en PNG (fond blanc, avec titre)">PNG</button>';
      barre.addEventListener('click', function (e) {
        var b = e.target.closest('button'); if (!b) return;
        e.stopPropagation();
        if (b.getAttribute('data-act') === 'agrandir') agrandir(gd);
        else telecharger(gd, 'cafe_' + (gd.id || 'graphique'), titreExport(gd));
      });
      gd.appendChild(barre);
    });
  }

  // Shiny recrée les graphiques à chaque changement : on surveille la page et on remet les boutons.
  var planifie = null;
  function planifier() {
    if (planifie) return;
    planifie = setTimeout(function () { planifie = null; ajouterOutils(); }, 250);
  }
  // Le script est chargé dans l'en-tête : on attend que la page existe avant de la surveiller.
  function demarrer() {
    ajouterOutils();
    new MutationObserver(planifier).observe(document.documentElement, { childList: true, subtree: true });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', demarrer);
  else demarrer();

  console.log('cafe_graphiques.js : chargé');
})();