// www/custom-handlers.js
// Cross-tab navigation handlers for the Offset Permanence Review app

$(document).on('shiny:sessioninitialized', function(event) {

  // Helper function to navigate to a study in the Database tab
  function goToStudy(digest) {
    if (digest) {
      Shiny.setInputValue('select_and_go', {
        digest: digest,
        tab: 'Database',
        nonce: Math.random()
      });
    }
  }

  // Custom handler from R server (session$sendCustomMessage)
  Shiny.addCustomMessageHandler('go_to_study_from_risk', function(message) {
    if (message && message.study_digest) {
      goToStudy(message.study_digest);
    }
  });

  // General purpose tab navigation from the server
  Shiny.addCustomMessageHandler('go_to_tab', function(message) {
    var target = message && message.tabId ? message.tabId : 'Database';
    Shiny.setInputValue('go_to_tab', target, {priority: "event"});
  });

  // Use a single event listener on the document body for efficiency
  $(document.body).on('click', function(e) {
    const el = e.target;

    // Handler for map marker popups
    const studyBtn = el.closest('.go-to-study-btn');
    if (studyBtn) {
      const studyTitle = studyBtn.getAttribute('data-title');
      if (studyTitle) {
        Shiny.setInputValue('go_to_study_from_map', {
          title: studyTitle,
          tab: 'Database',
          nonce: Math.random()
        });
      }
      return;
    }

    // Handler for database view buttons
    const dbBtn = el.closest('.db-view-btn');
    if (dbBtn) {
      goToStudy(dbBtn.getAttribute('data-digest'));
      return;
    }

    // Handler for study links within risk tables
    const studyLink = el.closest('.risk-study-link');
    if (studyLink) {
      goToStudy(studyLink.getAttribute('data-digest'));
      return;
    }

    // Handler for risk links (navigates to Risks tab)
    const riskLink = el.closest('.risk-link, .risk-link-category, .risk-link-domain');
    if (riskLink) {
      Shiny.setInputValue('selected_risk_info', {
        domain: riskLink.getAttribute('data-domain'),
        category: riskLink.getAttribute('data-category'),
        type: riskLink.getAttribute('data-type'),
        tab: 'Risks',
        nonce: Math.random()
      });
      return;
    }
  });
});
