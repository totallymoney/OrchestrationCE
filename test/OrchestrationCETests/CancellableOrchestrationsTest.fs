module OrchestrationCETests.CancellableOrchestrationsTest
open OrchestrationCE.CancellableOrchestration

type Context =
    { IsEmailSent: bool }
    
let mergeContexts { IsEmailSent = isEmailSent1 } { IsEmailSent = isEmailSent2 } =
    { IsEmailSent = isEmailSent1 || isEmailSent2 }
    
let cancellableOrchestration = CancellableOrchestrationBuilder(mergeContexts)
    

