angular.module('keymaerax.controllers').controller('ProofMessagesCtrl',
  function($rootScope, $scope, $uibModal) {

  $scope.proofMessage = {
    text: "",
    details: "",
    taskStepwiseRequest: undefined,
    isVisible: false
  }

  $rootScope.$on('agenda.updateWithoutProgress', function() {
    $scope.proofMessage.text = "The tactic did not make progress";
    $scope.proofMessage.isVisible = true;
  });

  $rootScope.$on('proof.message', function(event, message) {
    $scope.proofMessage.text = message.textStatus;
    $scope.proofMessage.details = message.errorThrown;
    $scope.proofMessage.causeMsg = message.causeMsg;
    $scope.proofMessage.taskStepwiseRequest = message.taskStepwiseRequest;
    $scope.proofMessage.isVisible = (message.textStatus !== "");
  });

  $rootScope.$on('agenda.loadError', function(event, userId, proofId) {
    var modalInstance = $uibModal.open({
      templateUrl: 'partials/prooftacticdialog.html',
      controller: 'ProofTacticDialogCtrl',
      size: 'fullscreen',
      resolve: {
        userid: function() { return userId; },
        proofid: function() { return proofId; },
        title: function() { return "Opening proof failed"; },
        message: function() { return "The last successfully recorded tactic steps are shown below."; }
      }
    });
  });

  $rootScope.$on('tactic.extractError', function(event, userId, proofId) {
    var modalInstance = $uibModal.open({
      templateUrl: 'partials/prooftacticdialog.html',
      controller: 'ProofTacticDialogCtrl',
      size: 'fullscreen',
      resolve: {
        userid: function() { return userId; },
        proofid: function() { return proofId; },
        title: function() { return "Extracting tactic failed"; },
        message: function() { return "The last successfully recorded tactic steps are shown below."; }
      }
    });
  });

})