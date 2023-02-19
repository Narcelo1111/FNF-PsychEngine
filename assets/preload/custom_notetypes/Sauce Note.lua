local limitLimitLol = 69
local limit = 0
function onCreatePost()
	precacheImage('ronald/sauceSplash')
	--Iterate over all notes
	for i = 0, getProperty('unspawnNotes.length')-1 do
		--Check if the note is an PicoSign
		if getPropertyFromGroup('unspawnNotes', i, 'noteType') == 'Sauce Note' then
			setPropertyFromGroup('unspawnNotes', i, 'texture', 'ronald/sauceNotess'); --Change texture
			setPropertyFromGroup('unspawnNotes', i, 'hitHealth', '0')
			setPropertyFromGroup('unspawnNotes', i, 'missHealth', '0')
			setPropertyFromGroup('unspawnNotes', i, 'hitCausesMiss', true);
			setPropertyFromGroup('unspawnNotes', i, 'noteSplashTexture', 'ronald/sauceSplash');

			setPropertyFromGroup('unspawnNotes', i, 'noteSplashHue', '0')
			setPropertyFromGroup('unspawnNotes', i, 'noteSplashSat', '0')
			setPropertyFromGroup('unspawnNotes', i, 'noteSplashBrt', '0')
			setPropertyFromGroup('unspawnNotes', i, 'colorSwap.hue', '0')
			setPropertyFromGroup('unspawnNotes', i, 'colorSwap.saturation', '0')
			setPropertyFromGroup('unspawnNotes', i, 'colorSwap.brightness', '0')

			setPropertyFromGroup('unspawnNotes', i, 'noteSplashAlpha', 1);
			if not getPropertyFromGroup('unspawnNotes', i, 'isSustainNote') then 
				setPropertyFromGroup('unspawnNotes', i, 'earlyHitMult', 0.7);	
			end
			if string.find(string.lower(getPropertyFromGroup('unspawnNotes', i, 'animation.curAnim.name')), 'end') then --fix sustain end lol
				setPropertyFromGroup('unspawnNotes', i, 'offsetX', getPropertyFromGroup('unspawnNotes', i, 'offsetX')+3);
			end
			setPropertyFromGroup('unspawnNotes', i, 'lateHitMult', 0.7);

			updateHitboxFromGroup('unspawnNotes', i)
			if getPropertyFromGroup('unspawnNotes', i, 'mustPress') then --Doesn't let Dad/Opponent notes get ignored
				setPropertyFromGroup('unspawnNotes', i, 'ignoreNote', true); --Miss has no penalties
			end
		end
	end

	--why bother checking the song lol
	--its just gonna load the limit text if the song has sauce notes
	makeLuaSprite('theLimit','ronald/limit',597 , 60);
	addLuaSprite('theLimit',true);
	scaleObject('theLimit', 0.5, 0.5);
	setObjectCamera('theLimit', 'hud')

	makeLuaText('limitCounter', limit, 800, 244, 90)
	setTextFont('limitCounter', 'limit.otf');
	setTextSize('limitCounter','40')
	setTextColor('limitCounter', 'FF0000')
	setTextBorder('limitCounter', 2, 'FFD800')
	addLuaText('limitCounter')

	screenCenter('theLimit', 'x')
	screenCenter('limitCounter', 'x')

	if (downscroll and not middlescroll) or (middlescroll and not downscroll) then 
		setProperty('theLimit.y', getProperty('theLimit.y')+500)
		setProperty('limitCounter.y', getProperty('limitCounter.y')+500)
	end
	if middlescroll and downscroll then 
		setProperty('theLimit.y', getProperty('theLimit.y')+80)
		setProperty('limitCounter.y', getProperty('limitCounter.y')+80)
	elseif middlescroll and not downscroll then 
		setProperty('theLimit.y', getProperty('theLimit.y')-50)
		setProperty('limitCounter.y', getProperty('limitCounter.y')-50)
	end
	--debugPrint('Script started!')
end
--[[
function noteMiss(id, noteData, noteType, isSustainNote)
    if noteType == 'Sauce Note' then
		if noteData == 0 then
			makeAnimatedLuaSprite('splash','ronald/sauceSplash', _G['defaultPlayerStrumX0']-72, _G['defaultPlayerStrumY0']-50);
			addAnimationByPrefix('splash','bop','note splash bbq',24,false)
			addLuaSprite('splash',false);
			scaleObject('splash', 0.7, 0.7);
			setObjectCamera('splash', 'other')
			objectPlayAnimation('splash', 'bop', true)
			runTimer('splash', 5/24);
		end
		if noteData == 1 then
			makeAnimatedLuaSprite('splash1','ronald/sauceSplash', _G['defaultPlayerStrumX1']-72, _G['defaultPlayerStrumY1']-50, 0);
			addAnimationByPrefix('splash1','bop','note splash spicy',24,false)
			addLuaSprite('splash1',false);
			scaleObject('splash1', 0.7, 0.7);
			setObjectCamera('splash1', 'other')
			objectPlayAnimation('splash1', 'bop', true)
			runTimer('splash1', 5/24);
		end
		if noteData == 2 then
			makeAnimatedLuaSprite('splash2','ronald/sauceSplash', _G['defaultPlayerStrumX2']-72, _G['defaultPlayerStrumY3']-50, 0);
			addAnimationByPrefix('splash2','bop','note splash mustard',24,false)
			addLuaSprite('splash2',false);
			scaleObject('splash2', 0.7, 0.7);
			setObjectCamera('splash2', 'other')
			objectPlayAnimation('splash2', 'bop', true)
			runTimer('splash2', 5/24);
		end
		if noteData == 3 then
			makeAnimatedLuaSprite('splash3','ronald/sauceSplash', _G['defaultPlayerStrumX3']-72, _G['defaultPlayerStrumY3']-50, 0);
			addAnimationByPrefix('splash3','bop','note splash sns',24,false)
			addLuaSprite('splash3',false);
			scaleObject('splash3', 0.7, 0.7);
			setObjectCamera('splash3', 'other')
			objectPlayAnimation('splash3', 'bop', true)
			runTimer('splash3', 5/24);
		end
    end
end

function onTimerCompleted(tag)
    if tag == 'splash' then
        removeLuaSprite('splash');
    end
	if tag == 'splash1' then
        removeLuaSprite('splash1');
    end
	if tag == 'splash2' then
        removeLuaSprite('splash2');
    end
	if tag == 'splash3' then
        removeLuaSprite('splash3');
    end
end]]--


function onUpdate()
    if limit == limitLimitLol then
        setProperty('health', 0)
    end
	setTextString('limitCounter', limit)
	
end

function noteMiss(id, noteData, noteType, isSustainNote)
    if noteType == 'Sauce Note' then
        limit = limit+1
	end
end

function onEvent(name, value1, value2)
	if name == 'Limit Set' then
		limitValue = tonumber(value1);
		if limitValue == 0 then
			setProperty('health', 0)
		else
			limitLimitLol = limitValue
		end
	end
end