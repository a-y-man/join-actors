package join_actors.examples.payment_microservice

enum PaymentEvent:
  case PaymentRequested(id: Int)
  case MerchantValidated(id: Int)
  case TokenConsumed(id: Int)
  case CustomerValidated(id: Int)
  case PaymentSuceeded(id: Int)

  case TokenGenerationRequested(id: Int)
  case TokenGenerated(id: Int)

  case ExternalPaymentRequest()
  case ExternalTokenGenerationRequest()

  case Shutdown()
