{-# LANGUAGE OverloadedStrings #-}
import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract "Alice" "Bob" "Charlie" $ ConstantParam "Amount"
contract :: Party -> Party -> Party -> Value -> Contract
contract party1 party2 mediator amount =
    mediatorMakesDeposit mediator (doubleAmount amount) $
    partiesMakeDeposits party1 party2 amount $
    mediatorMakesChoice party1 party2 mediator amount


choiceId :: Party -> ChoiceId
choiceId = ChoiceId "Winner"

mediatorDepositTimeout, partyDepositTimeout, choiceTimeout :: Timeout
mediatorDepositTimeout = TimeParam "Mediator deadline"
partyDepositTimeout = TimeParam "Parties deadline"
choiceTimeout = TimeParam "Choice deadline"

doubleAmount :: Value -> Value
doubleAmount amount = AddValue amount amount

mediatorMakesDeposit :: Party -> Value -> Contract -> Contract
mediatorMakesDeposit mediator mediatorAmount continuation =
    When
        [Case
            (Deposit mediator mediator ada mediatorAmount)
            continuation
        ]
        mediatorDepositTimeout Close

partiesMakeDeposits :: Party -> Party -> Value -> Contract -> Contract
partiesMakeDeposits party1 party2 partyAmount continuation =
    When
        [ orderVariant party1 party2 continuation
        , orderVariant party2 party1 continuation
        ]
        partyDepositTimeout Close
    where
        orderVariant :: Party -> Party -> Contract -> Case
        orderVariant p1 p2 continuation =
            Case
                (Deposit p1 p1 ada partyAmount)
                (When
                    [Case
                        (Deposit p2 p2 ada partyAmount)
                        continuation
                    ]
                    partyDepositTimeout
                    Close
                )

mediatorMakesChoice :: Party -> Party -> Party -> Value -> Contract
mediatorMakesChoice party1 party2 mediator partyAmount =
    When
        [Case
            (Choice (choiceId mediator) [Bound 1 2] )
            (If
                (ValueEQ
                    (ChoiceValue $ choiceId mediator)
                    (Constant 1)
                )
                (Pay party2 (Account party1) ada partyAmount Close)
                (Pay party1 (Account party2) ada partyAmount Close)
            )
        ]
        choiceTimeout
        (choiceWasNotMade party1 party2 mediator partyAmount)

choiceWasNotMade ::  Party -> Party -> Party -> Value -> Contract
choiceWasNotMade party1 party2 mediator partyAmount =
    Pay mediator (Account party1) ada partyAmount $
    Pay mediator (Account party2) ada partyAmount Close

