addJitter <-
function(trellobj=trellis.last.object(), amount=.025) {update(trellobj, jitter.x=T, jitter.y=T, amount=amount)}
