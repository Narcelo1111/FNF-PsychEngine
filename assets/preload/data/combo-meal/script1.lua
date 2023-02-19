--Arrows Sliding Repeatley

function onUpdate(elapsed)
songPos = getSongPosition()
local currentBeat = (songPos/5000)*(curBpm/60)

--Player Notes

noteTweenX(defaultPlayerStrumX0, 4, defaultPlayerStrumX0 - 40*math.sin((currentBeat+4*0.25)*math.pi), 0.5)
noteTweenX(defaultPlayerStrumX1, 5, defaultPlayerStrumX1 - 40*math.sin((currentBeat+5*0.25)*math.pi), 0.5)
noteTweenX(defaultPlayerStrumX2, 6, defaultPlayerStrumX2 - 40*math.sin((currentBeat+6*0.25)*math.pi), 0.5)
noteTweenX(defaultPlayerStrumX3, 7, defaultPlayerStrumX3 - 40*math.sin((currentBeat+7*0.25)*math.pi), 0.5)

--Opponent Notes

noteTweenX(defaultOpponentStrumX0, 0, defaultOpponentStrumX0 - 40*math.sin((currentBeat+0*0.25)*math.pi), 0.5)
noteTweenX(defaultOpponentStrumX1, 1, defaultOpponentStrumX1 - 40*math.sin((currentBeat+1*0.25)*math.pi), 0.5)
noteTweenX(defaultOpponentStrumX2, 2, defaultOpponentStrumX2 - 40*math.sin((currentBeat+2*0.25)*math.pi), 0.5)
noteTweenX(defaultOpponentStrumX3, 3, defaultOpponentStrumX3 - 40*math.sin((currentBeat+3*0.25)*math.pi), 0.5)
end