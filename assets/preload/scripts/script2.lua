

function onCreate()
	--makeLuaText('gameplaynotfinal', 'Gameplay not final.', 800, -240, 670)
	--setTextSize('gameplaynotfinal','25')
	--addLuaText('gameplaynotfinal')


end
function onCreatePost()
	setTextFont('scoreTxt', 'RoA Small.ttf')
	--setTextBorder('scoreTxt', 3, '0xFF000000')
	setProperty('scoreTxt.borderSize', 2.5)
	setProperty('scoreTxt.antialiasing', false)

	setTextFont('timeTxt', 'RoA Small.ttf')
	setProperty('timeTxt.borderSize', 2.5)
	setProperty('timeTxt.antialiasing', false)

	local noteSkinName = 'sauceNotess'
	if getPropertyFromClass('ClientPrefs', 'sauceNoteSkin') == 'Szechuan Sauce' then 
		noteSkinName = 'szechNotess'
	end

	if getPropertyFromClass('PlayState', 'SONG.player2') == 'woh' or getPropertyFromClass('PlayState', 'SONG.player2') == 'ronald-lv9' then 
		for i = 0, getProperty('unspawnNotes.length')-1 do 
			if not getPropertyFromGroup('unspawnNotes', i, 'mustPress') and getPropertyFromGroup('unspawnNotes', i, 'noteType') ~= 'Sauce Note' then 
				setPropertyFromGroup('unspawnNotes', i, 'texture', 'ronald/'..noteSkinName); --force to sauce note
				if string.find(string.lower(getPropertyFromGroup('unspawnNotes', i, 'animation.curAnim.name')), 'end') then --fix sustain end lol
					setPropertyFromGroup('unspawnNotes', i, 'offsetX', getPropertyFromGroup('unspawnNotes', i, 'offsetX')+3);
				end
				setPropertyFromGroup('unspawnNotes', i, 'noteSplashHue', '0')
				setPropertyFromGroup('unspawnNotes', i, 'noteSplashSat', '0')
				setPropertyFromGroup('unspawnNotes', i, 'noteSplashBrt', '0')
				setPropertyFromGroup('unspawnNotes', i, 'colorSwap.hue', '0')
				setPropertyFromGroup('unspawnNotes', i, 'colorSwap.saturation', '0')
				setPropertyFromGroup('unspawnNotes', i, 'colorSwap.brightness', '0')
			end
		end
	end
end

local songData = {
	{'the idea', 'The Idea', 'Composed by Gioboi'},
	{'combo meal', 'Combo Meal', 'Composed by Waddle'},
	{'deaf to all but the ronald', 'Deaf to All but the Ronald', 'Cover by Lean'},
}

function onSongStart()
	local textWidth = 300
	local textY = 150
	local textSize = 20
	local text = 'no song found'
	for i = 0,#songData-1 do 
		if string.lower(songName) == songData[i+1][1] then 
			text = songData[i+1][2]..'\n'..songData[i+1][3]
		end
	end

	makeLuaText('popupText', text, textWidth, 5-textWidth, textY+5)
	setTextSize('popupText', textSize)
	setTextFont('popupText', 'limit.otf');

	makeLuaSprite('popupBox', '', 0, textY)
	makeGraphic('popupBox', textWidth, getProperty('popupText.height')+textSize, '0xFF000000')
	setProperty('popupBox.x', -textWidth)

	doTweenX('popup', 'popupBox', 0, 0.5, 'cubeInOut')
	runTimer('popupEnd', 4)
	setObjectCamera('popupBox', 'hud')
	addLuaSprite('popupBox', true)
	setProperty('popupBox.alpha', 0.7)


	addLuaText('popupText')
	doTweenX('popupText', 'popupText', 5, 0.5, 'cubeInOut')	
end

function onTimerCompleted(tag, loops, loopsleft)
	if tag == 'popupEnd' then 
		doTweenX('popupEnd', 'popupBox', -getProperty('popupBox.width')-500, 0.5, 'cubeInOut')
		doTweenX('popupTextEnd', 'popupText', -getProperty('popupBox.width')-500, 0.5, 'cubeInOut')
	end
end

function onEvent(name, val1, val2)

	if name == 'Play Animation' then 
		setProperty('vocals.volume', 1) --should fix laughs if you missed at the end of a turn (can happen on cm erect)
	end
end