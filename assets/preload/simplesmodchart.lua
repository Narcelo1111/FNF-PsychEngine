--"simple" psych lua modchart template by TheZoroForce240
--insprired by other templates (and notitg/itg of course)
--simple for actual modcharts, complex for generic fnf modcharts, can be really powerful if you know what youre doing
--made originally for final timeout new modchart (vs mart)

--i guess its not as simple anymore lol

--SETTINGS--

local modchart = true
--simple toggle off just in case

local useShaders = false
--only use if on 0.6.3+ (when it releases lol) or youre using a version of psych from the github where shaders are available

local legacyPsychMode = false
--disables any hscript so this can work on older psych versions (0.5.2), enabling can also help with performance but disables z layering

local useNoteQuants = false --(requires 0.6.2+)
--notes get colored based on beat, can make jumps in modcharts easier to read
--doesnt work on pixel stages or songs with different note assets

local layerNotesWithStrums = false --(requires 0.6.2+)
--changes z layering so strums can be over notes
--affects performance

local allowCrossScriptModifiers = false --(requires 0.6.2+)
--allows you to add custom modifiers via a script at the cost of some performance



--NOTE PATH SETTINGS--

--note paths are pretty laggy and can only be turned on via modchart (by changing the alpha)
--might try to make a sustain renderer shader in the future to improve performance but rn its reusing the sustain angling code for it

local useNotePaths = false 
--toggle for note paths

local pathCount = 10
--how many segments per lane

local pathSize = 130
--height of each segment


------------




--these get set up automatically now lol
--also available globally (if 0.6.2+)
local keyCount = 4
local arrowSize = 112 --160 * 0.7, usually wanna change if using more keys

--dont mess with these
local defaultSusScaleY = -1 --store scaley for all sustains
local defaultSusEndScaleY = -1
local scrollSwitch = 520 --height to move to when reverse
local rad = math.pi/180;

function setupModifiers()
	for i = 0,(keyCount*2)-1 do 
		makeLuaSprite('strumOffset'..i, '', 0, 0)
		makeLuaSprite('scaleMulti'..i, '', 1, 1)
		makeLuaSprite('reverse'..i, '', 0, 0)
		makeLuaSprite('confusion'..i, '', 0, 0)


		makeLuaSprite('incomingAngle'..i, '', 0, 0)
		makeLuaSprite('noteRot'..i, '', 0, 0)

		if useNotePaths then 
			for j = 0,pathCount do 
				makeLuaSprite(i..'NotePath'..j, '', -500, 0)
				makeGraphic(i..'NotePath'..j, 15, pathSize, '0xFFFFFFFF')
				setObjectCamera(i..'NotePath'..j, 'hud')
				setProperty(i..'NotePath'..j..'.alpha', 0)
				addLuaSprite(i..'NotePath'..j, false)
			end
		end

	end

	makeLuaSprite('globalStrumOffset', '', 0, 0) --general x,y,z movement
	makeLuaSprite('playerStrumOffset', '', 0, 0)
	makeLuaSprite('opponentStrumOffset', '', 0, 0)

	makeLuaSprite('incomingAngle', '', 0, 0) --angle that notes come at (x,y)
	makeLuaSprite('playerIncomingAngle', '', 0, 0)
	makeLuaSprite('opponentIncomingAngle', '', 0, 0)

	makeLuaSprite('noteRot', '', 0, 0) --spins strums around the center of the screen (x,y)
	makeLuaSprite('playerNoteRot', '', 0, 0) --changing y also changes incoming angle to match
	makeLuaSprite('opponentNoteRot', '', 0, 0)
	
	makeLuaSprite('screenRot', '', 0, 0) --spins screen using a raymarcher shader (x,y)

	makeLuaSprite('brake', '', 0, 0) --slows notes down near strumline (y)
	makeLuaSprite('playerBrake', '', 0, 0)
	makeLuaSprite('opponentBrake', '', 0, 0)

	makeLuaSprite('boost', '', 0, 0) --speeds up notes down near strumline (y)
	makeLuaSprite('playerBoost', '', 0, 0)
	makeLuaSprite('opponentBoost', '', 0, 0)

	makeLuaSprite('twist', '', 0, 0) --noteRot but changes for notes as they move, y on low value is pretty cool
	
	--using lua sprites so you can tween lol
	--changing angle changes the z, not actual angle, to change note angle use confusion
	makeLuaSprite('tipsy', '', 0, 0) 
	makeLuaSprite('drunk', '', 0, 0)
	makeLuaSprite('tipsySpeed', '', 1, 1)
	makeLuaSprite('drunkSpeed', '', 1, 1)


	makeLuaSprite('playerNotePathAlpha', '', 0, 0)
	setProperty('playerNotePathAlpha.alpha', 0)
	makeLuaSprite('opponentNotePathAlpha', '', 0, 0)
	setProperty('opponentNotePathAlpha.alpha', 0)

	makeLuaSprite('dark', '', 0, 0) --strum alpha
	makeLuaSprite('stealth', '', 0, 0) --note alpha
	setProperty('dark.alpha', 0)
	setProperty('stealth.alpha', 0)

	makeLuaSprite('sudden', '', 0, 0)
	makeLuaSprite('hidden', '', 0, 0)
	setProperty('sudden.alpha', 0)
	setProperty('hidden.alpha', 0)

	--setProperty('hidden.y', -0.7)

	

	makeLuaSprite('scale', '', 0.7, 0.7) --0.7 = default scale
	makeLuaSprite('confusion', '', 0, 0) --angle

	makeLuaSprite('reverse', '', 0, 0) --only y does stuff


	if not legacyPsychMode then --for cross script stuff
		runHaxeCode([[
			game.setOnLuas("currentXOffset", 0);
			game.setOnLuas("currentYOffset", 0);
			game.setOnLuas("currentZOffset", 0);
			game.setOnLuas("currentAngleOffset", 0);
			game.setOnLuas("currentAlphaOffset", 1);
		]])
	end
end

--a lot of modifier math comes from here btw
--https://github.com/openitg/openitg/blob/master/src/ArrowEffects.cpp


function runModifiers(data, curPos)
	--this is where mod math gets applied to strums/notes
	local xOffset = 0
	local yOffset = 0
	local zOffset = 0
	local angle = 0
	local alpha = 1

	xOffset = xOffset + getProperty('strumOffset'..data..'.x') --add strum offsets
	yOffset = yOffset + getProperty('strumOffset'..data..'.y')
	zOffset = zOffset + getProperty('strumOffset'..data..'.angle') --using angle because

	xOffset = xOffset + getProperty('globalStrumOffset.x') --add strum offsets
	yOffset = yOffset + getProperty('globalStrumOffset.y')
	zOffset = zOffset + getProperty('globalStrumOffset.angle')

	if data < keyCount then 
		xOffset = xOffset + getProperty('opponentStrumOffset.x') --add strum offsets
		yOffset = yOffset + getProperty('opponentStrumOffset.y')
		zOffset = zOffset + getProperty('opponentStrumOffset.angle')
	else 
		xOffset = xOffset + getProperty('playerStrumOffset.x') --add strum offsets
		yOffset = yOffset + getProperty('playerStrumOffset.y')
		zOffset = zOffset + getProperty('playerStrumOffset.angle')
	end

	--drunk
	if getProperty('drunk.x') ~= 0 then 
		xOffset = xOffset + getProperty('drunk.x') * (math.cos( ((songPosition*0.001) + ((data%keyCount)*0.2) + (curPos*0.45)*(10/screenHeight)) * (getProperty('drunkSpeed.x')*0.2)) * arrowSize*0.5);
	end
	if getProperty('drunk.y') ~= 0 then 
		yOffset = yOffset + getProperty('drunk.y') * (math.cos( ((songPosition*0.001) + ((data%keyCount)*0.2) + (curPos*0.45)*(10/screenHeight)) * (getProperty('drunkSpeed.y')*0.2)) * arrowSize*0.5);
	end
	if getProperty('drunk.angle') ~= 0 then 
		zOffset = zOffset + getProperty('drunk.angle') * (math.cos( ((songPosition*0.001) + ((data%keyCount)*0.2) + (curPos*0.45)*(10/screenHeight)) * (getProperty('drunkSpeed.angle')*0.2)) * arrowSize*0.5);
	end

	--tipsy
	if getProperty('tipsy.x') ~= 0 then 
		xOffset = xOffset + getProperty('tipsy.x') * ( math.cos( songPosition*0.001 *(1.2) + (data%keyCount)*(2.0) + getProperty('tipsySpeed.x')*(0.2) ) * arrowSize*0.4 );
	end
	if getProperty('tipsy.y') ~= 0 then 
		yOffset = yOffset + getProperty('tipsy.y') * ( math.cos( songPosition*0.001 *(1.2) + (data%keyCount)*(2.0) + getProperty('tipsySpeed.y')*(0.2) ) * arrowSize*0.4 );
	end
	if getProperty('tipsy.angle') ~= 0 then 
		zOffset = zOffset + getProperty('tipsy.angle') * ( math.cos( songPosition*0.001 *(1.2) + (data%keyCount)*(2.0) + getProperty('tipsySpeed.angle')*(0.2) ) * arrowSize*0.4 );
	end

	--reverse (scroll flip)
	if getProperty('reverse.y') ~= 0 or getProperty('reverse'..data..'.y') ~= 0 then 
		yOffset = yOffset + scrollSwitch * (getProperty('reverse.y') + getProperty('reverse'..data..'.y'))
	end

	--confusion (note angle)
	if getProperty('confusion.angle') ~= 0 or getProperty('confusion'..data..'.angle') ~= 0 then 
		angle = angle + getProperty('confusion.angle') + getProperty('confusion'..data..'.angle')
	end

	if getProperty('stealth.alpha') ~= 0 then --notes
		if curPos ~= 0 then 
			alpha = alpha - getProperty('stealth.alpha')
		end
	end

	if getProperty('dark.alpha') ~= 0 then --strums
		if curPos == 0 then 
			alpha = alpha - getProperty('dark.alpha')
		end
	end

	if getProperty('hidden.alpha') ~= 0 then
		if curPos ~= 0 then 
			local fHiddenVisibleAdjust = scale( (-curPos / songSpeed), GetHiddenStartLine(), GetHiddenEndLine(), -1, 0);
			fHiddenVisibleAdjust = clamp( fHiddenVisibleAdjust, -1, 0 );
			alpha = alpha + getProperty('hidden.alpha') * fHiddenVisibleAdjust;
		end
	end
	if getProperty('sudden.alpha') ~= 0 then
		if curPos ~= 0 then 
			local fSuddenVisibleAdjust = scale( (-curPos / songSpeed), GetSuddenStartLine(), GetSuddenEndLine(), -1, 0);
			fSuddenVisibleAdjust = clamp( fSuddenVisibleAdjust, -1, 0 );
			alpha = alpha + getProperty('sudden.alpha') * fSuddenVisibleAdjust;
		end
	end
		

	--add any custom modifiers here lol 
	--though you can do it cross script with the hscript function

	if not legacyPsychMode and allowCrossScriptModifiers then 
		runHaxeCode([[
			var data = ]]..data..[[;
			var curPos = ]]..curPos..[[;
			var arrowSize = ]]..arrowSize..[[;
			var keyCount = ]]..keyCount..[[;
	
			var func = game.variables["customModifierFunction"]; //cross script mods!!!!!!
			func(data, curPos, arrowSize, keyCount);
		]])

		xOffset = xOffset + currentXOffset
		yOffset = yOffset + currentYOffset
		zOffset = zOffset + currentZOffset
		angle = angle + currentAngleOffset
		alpha = alpha * currentAlphaOffset
	end

	return {xOffset, yOffset, zOffset/1000, angle, alpha}
	--do divide 1000 on z so it matches more closely to the other axis
end

function runPosModifiers(data, curPos)

	local yOffset = 0

	if getProperty('boost.y') ~= 0 then --speed up

		local fYOffset = -curPos / songSpeed --idk why its minus it just is
		local fEffectHeight = screenHeight
		local fNewYOffset = fYOffset * 1.5 / ((fYOffset+fEffectHeight/1.2)/fEffectHeight); 
		local fAccelYAdjust = getProperty('boost.y')  * (fNewYOffset - fYOffset);
		fAccelYAdjust = clamp(fAccelYAdjust*songSpeed, -400, 400);
		yOffset = yOffset - (fAccelYAdjust);
	end

	if getProperty('brake.y') ~= 0 then --slow down

		local fYOffset = -curPos / songSpeed
		local fEffectHeight = screenHeight
		local fScale = scale(fYOffset, 0, fEffectHeight, 0, 1);
		local fNewYOffset = fYOffset * fScale; 
		local fBrakeYAdjust = getProperty('brake.y') * (fNewYOffset - fYOffset);
		fBrakeYAdjust = clamp( fBrakeYAdjust, -400, 400 );
		yOffset = yOffset - fBrakeYAdjust*songSpeed;
	end

	curPos = curPos + yOffset

	return curPos
end

function getIncomingAngleX(data, curPos)
	local playerSpecific = getProperty('playerIncomingAngle.x')
	if data < keyCount then 
		playerSpecific = getProperty('opponentIncomingAngle.x')
	end
	return (getProperty('incomingAngle'..data..'.x') + getProperty('incomingAngle.x') + playerSpecific)
end

function getIncomingAngleY(data, curPos)
	local playerSpecific = getProperty('playerIncomingAngle.y')
	if data < keyCount then 
		playerSpecific = getProperty('opponentIncomingAngle.y')
	end
	return (getProperty('incomingAngle'..data..'.y') + getProperty('incomingAngle.y') + playerSpecific - getNoteRotY(data, curPos))
end

function getNoteRotX(data, curPos)
	local playerSpecific = getProperty('playerNoteRot.x')
	if data < keyCount then 
		playerSpecific = getProperty('opponentNoteRot.x')
	end
	
	if getProperty('twist.x') ~= 0 then 
		playerSpecific = playerSpecific + math.abs(curPos)*getProperty('twist.x')*0.1
	end
	return (getProperty('noteRot'..data..'.x') + getProperty('noteRot.x') + playerSpecific)
end

function getNoteRotY(data, curPos)
	local playerSpecific = getProperty('playerNoteRot.y')
	if data < keyCount then 
		playerSpecific = getProperty('opponentNoteRot.y')
	end
	if getProperty('twist.y') ~= 0 then 
		playerSpecific = playerSpecific + math.abs(curPos)*getProperty('twist.y')*0.1
	end
	return (getProperty('noteRot'..data..'.y') + getProperty('noteRot.y') + playerSpecific)
end

function getScreenRotX(player)
	return (getProperty('screenRot.x'))%360
end

function getScreenRotY(player)
	return (getProperty('screenRot.y'))%360
end

function getScaleX(data, curPos)
	return getProperty('scale.x') * getProperty('scaleMulti'..data..'.x')
end

function getScaleY(data, curPos)
	return getProperty('scale.y') * getProperty('scaleMulti'..data..'.y')
end

function getNoteDist(data, curPos, dist)
	if getProperty('reverse.y') ~= 0 then 
		dist = dist * (1-(getProperty('reverse.y')*2))		
	end
	if getProperty('reverse'..data..'.y') ~= 0 then 
		dist = dist * (1-(getProperty('reverse'..data..'.y')*2))		
	end

	return dist
end

local FADE_DIST_Y = 60 
local CENTER_LINE_Y = 720/2

function GetHiddenSudden()
	return getProperty('hidden.alpha') * getProperty('sudden.alpha')
end

function GetHiddenEndLine()
	return CENTER_LINE_Y + 
		FADE_DIST_Y * scale( GetHiddenSudden(), 0, 1, 1, 1.25) + 
		CENTER_LINE_Y * getProperty('hidden.y')
end

function GetHiddenStartLine()
	return CENTER_LINE_Y + 
		FADE_DIST_Y * scale( GetHiddenSudden(), 0, 1, 0.0, 0.25) + 
		CENTER_LINE_Y * getProperty('hidden.y')
end

function GetSuddenEndLine()
	return CENTER_LINE_Y + 
		FADE_DIST_Y * scale( GetHiddenSudden(), 0, 1, 0.0, 0.25) + 
		CENTER_LINE_Y * getProperty('sudden.y')
end

function GetSuddenStartLine()
	return CENTER_LINE_Y + 
		FADE_DIST_Y * scale( GetHiddenSudden(), 0, 1, 1.0, 1.25) + 
		CENTER_LINE_Y * getProperty('sudden.y')
end




function onCreatePost()

	if getPropertyFromClass('PlayState', 'isPixelStage') then 
		useNoteQuants = false
	end


	if useNoteQuants and not legacyPsychMode then 
		addLuaScript('noteQuants')
	end




	if not legacyPsychMode then 

		keyCount = getProperty('playerStrums.length')
		arrowSize = getPropertyFromClass('Note', 'swagWidth')

		runHaxeCode('game.setOnLuas("keyCount", '..keyCount..')')
		runHaxeCode('game.setOnLuas("arrowSize", '..arrowSize..')')

		addHaxeLibrary('Math')
		addHaxeLibrary('FlxAngle')
	end
	
	setupModifiers()
	--luaDebugMode = true
	if not legacyPsychMode then 
		setupCamera()
	end
	

	if getPropertyFromClass('PlayState', 'isPixelStage') then 
		setProperty('scale.x', 0.7 * 8.57)
		setProperty('scale.y', 0.7 * 8.57)
	end
	

	--setProperty('drunk.x', 0.5)
	--setProperty('tipsySpeed.angle', 1)

	if downscroll then 
		scrollSwitch = -520
	end


	--initLuaShader('sustainRenderer')
	--setSpriteShader('dad', 'sustainRenderer')
	
	

	--runHaxeCode(raymarchcode)


	modchart = getPropertyFromClass('ClientPrefs', 'modcharts')
	if not modchart then 
		return 
	end


	
	--setSpriteShader('camHUD', 'raymarch')
	--setShaderFloat('raymarch', 'rotX', 15*(math.pi/180))

	

	--setShaderFloat('raymarch', 'rotX', 15*(math.pi/180))
end

local timeStopStart = 0
local timeStopEnd = 0
local stopTime = false

function onEvent(tag, val1, val2)

	if tag == 'resetX' then 
		resetX(tonumber(val1), val2)
	elseif tag == 'resetY' then 
		resetY(tonumber(val1), val2)
	elseif tag == 'resetZ' then 
		resetZ(tonumber(val1), val2)
	elseif tag == 'invert' then 
		invert(tonumber(val1), val2)
	elseif tag == 'flip' then 
		flip(tonumber(val1), val2)
	elseif tag == 'easeModifierX' then 
		local nameAndVal = val1:split(",")
		local timeAndEase = val2:split(",")
		doTweenX(nameAndVal[1]..'.x', nameAndVal[1], tonumber(nameAndVal[2]), tonumber(timeAndEase[1]), timeAndEase[2])
	elseif tag == 'easeModifierY' then 
		local nameAndVal = val1:split(",")
		local timeAndEase = val2:split(",")
		doTweenY(nameAndVal[1]..'.y', nameAndVal[1], tonumber(nameAndVal[2]), tonumber(timeAndEase[1]), timeAndEase[2])
	elseif tag == 'easeModifierZ' then 
		local nameAndVal = val1:split(",")
		local timeAndEase = val2:split(",")
		doTweenAngle(nameAndVal[1]..'.z', nameAndVal[1], tonumber(nameAndVal[2]), tonumber(timeAndEase[1]), timeAndEase[2])
	elseif tag == 'setModifierX' then 
		setProperty(val1..'.x', tonumber(val2))
	elseif tag == 'setModifierY' then 
		setProperty(val1..'.y', tonumber(val2))
	elseif tag == 'setModifierZ' then 
		setProperty(val1..'.angle', tonumber(val2))
	elseif tag == 'allowCrossScriptModifiers' then 
		allowCrossScriptModifiers = true
	elseif tag == 'useShaders' then 
		useShaders = true
	elseif tag == 'legacyPsychMode' then 
		legacyPsychMode = true
	elseif tag == 'layerNotesWithStrums' then 
		layerNotesWithStrums = true
	elseif tag == 'useNotePaths' then 
		useNotePaths = true
	elseif tag == 'disableModchart' then 
		modchart = false

	elseif tag == 'setTimeStop' then 
		timeStopStart = tonumber(val1)
		timeStopEnd = tonumber(val2)
	end
end

function string:split( inSplitPattern, outResults ) -- from here, code isnt mine, https://stackoverflow.com/questions/19262761/lua-need-to-split-at-comma
    if not outResults then
      outResults = { }
    end
    local theStart = 1
    local theSplitStart, theSplitEnd = string.find( self, inSplitPattern, theStart )
    while theSplitStart do
      table.insert( outResults, string.sub( self, theStart, theSplitStart-1 ) )
      theStart = theSplitEnd + 1
      theSplitStart, theSplitEnd = string.find( self, inSplitPattern, theStart )
    end
    table.insert( outResults, string.sub( self, theStart ) )
    return outResults
end

function resetX(time, ease)
	for i = 0,(keyCount*2)-1 do
		doTweenX(i..'x', 'strumOffset'..i, 0, time, ease)
	end
end
function resetY(time, ease)
	for i = 0,(keyCount*2)-1 do
		doTweenY(i..'y', 'strumOffset'..i, 0, time, ease)
	end
end
function resetZ(time, ease)
	for i = 0,(keyCount*2)-1 do
		doTweenAngle(i..'z', 'strumOffset'..i, 0, time, ease)
	end
end

function invert(time, ease)
	for i = 0,(keyCount*2)-1 do
		local invert = -1
		if i % 2 == 0 then 
			invert = 1
		end
		doTweenX(i..'x', 'strumOffset'..i, arrowSize*invert, time, ease)
	end
end
function flip(time, ease)
	--for i = 0,7 do
	--	doTweenX(i..'x', 'strumOffset'..i, arrowSize * 2 * (1.5 - (i%4)), time, ease)
	--end
	doTweenX('0x', 'strumOffset0', arrowSize * 3, time, ease)
	doTweenX('1x', 'strumOffset1', arrowSize * 1, time, ease)
	doTweenX('2x', 'strumOffset2', arrowSize * -1, time, ease)
	doTweenX('3x', 'strumOffset3', arrowSize * -3, time, ease)

	doTweenX('4x', 'strumOffset4', arrowSize * 3, time, ease)
	doTweenX('5x', 'strumOffset5', arrowSize * 1, time, ease)
	doTweenX('6x', 'strumOffset6', arrowSize * -1, time, ease)
	doTweenX('7x', 'strumOffset7', arrowSize * -3, time, ease)
	
end

local copyZ = true

function onUpdatePost(elapsed)

	if not modchart or getProperty('isDead') then 
		return 
	end

	if not legacyPsychMode then 

		--how z layering works

		--put both sprite and z val inside an array and then push into "ZLayeredStuffs"
		--at the start of each frame it resets "ZLayeredStuffs" by removing all sprites placed last frame
		--then at the end of update post it sorts the array / note group and adds all the sprites back and whatever

		--also strumlinenotes is hidden but not removed because the ratings dont show, and has active set to false to not update the strums twice
		--sorting strumlinenotes would cause issues since some things depend on indexing it
		
		local shitcode = [[
			for (i in 0...game.variables["ZLayeredStuffs"].length)
			{
				game.remove(game.variables["ZLayeredStuffs"][i][0]);
			}
			game.variables["ZLayeredStuffs"] = [];
			
		]]
		runHaxeCode(shitcode) --reset z layering shit
	end

	local noteCount = getProperty('notes.length');
	fakeCrochet = (60 / bpm) * 1000
	songPosition = getSongPosition() --small optimizations i guess
	startSongSpeed = getProperty('scrollSpeed')
	songSpeed = getProperty('songSpeed')

	for i = 0,(keyCount*2)-1 do 	
		setupStrumPosition(i)
	end

	for i = 0, noteCount-1 do 
		setupNotePosition(i)
		if getPropertyFromGroup('notes', i, 'isSustainNote') then 
			setupSustainAngle(i)
		end	
	end

	
	if timeStopEnd < songPosition and timeStopStart > 0 then 
		timeStopStart = 0 --reset
	end
	setProperty('timeStopStart', timeStopStart)
	setProperty('timeStopEnd', timeStopEnd)

	if useNotePaths then 
		for i = 0, (keyCount*2)-1 do
			local shouldRender = false 

			if i < keyCount then 
				if getProperty('opponentNotePathAlpha.alpha') > 0 then 
					shouldRender = true
				end
			else 
				if getProperty('playerNotePathAlpha.alpha') > 0 then 
					shouldRender = true
				end
			end

			if shouldRender then 
				RenderNotePaths(i)
			end		
		end
	end
	
	if not legacyPsychMode then 
		
		runHaxeCode('game.variables["noteCamera"].x = game.camHUD.x;') --make sure cameras match lol
		runHaxeCode('game.variables["noteCamera"].y = game.camHUD.y;')
		runHaxeCode('game.variables["noteCamera"].angle = game.camHUD.angle;')
		runHaxeCode('game.variables["noteCamera"].zoom = game.camHUD.zoom;')

		if useShaders then 
			runHaxeCode('game.variables["raymarchShader"].setFloat("rotX", '..(getScreenRotX()*rad)..');')
			runHaxeCode('game.variables["raymarchShader"].setFloat("rotY", '..(getScreenRotY()*rad)..');')
		end

		--z layering bullshit
		local layerWithoutNotes = [[

			game.variables["ZLayeredStuffs"].sort(function(a, b)
			{
				if (a[1] < b[1]) {
					return -1;
				}
				else if (a[1] > b[1]) {
				return 1;
				} else {
				return 0;
				}
			});

			game.remove(game.notes);
			for (i in 0...game.variables["ZLayeredStuffs"].length)
			{
				game.add(game.variables["ZLayeredStuffs"][i][0]);
			}
			game.add(game.notes);

			
			game.notes.members.sort(function(a, b) //doing this because it gives better performance!
			{
				if (a.distance < b.distance) {
					return -1;
				}
				else if (a.distance > b.distance) {
				return 1;
				} else {
				return 0;
				}
			});
			

		]]
		local layerWithNotes = [[
			game.variables["ZLayeredStuffs"].sort(function(a, b)
			{
				if (a[1] < b[1]) {
					return -1;
				}
				else if (a[1] > b[1]) {
				return 1;
				} else {
				return 0;
				}
			});

			for (i in 0...game.variables["ZLayeredStuffs"].length)
			{
				game.add(game.variables["ZLayeredStuffs"][i][0]);
			}

		]]
		if layerNotesWithStrums then 
			runHaxeCode(layerWithNotes)
		else 
			runHaxeCode(layerWithoutNotes)
		end
	end
end




--[[
function onDrawPost()
	--debugPrint('draw lol')
	if getProperty('paused') then 
		return
	end

	for i = 0,(keyCount*2)-1 do 
		setPropertyFromGroup('strumLineNotes', i, 'x', getPropertyFromGroup('strumLineNotes', i, 'x')-300)
		
		local shitcode = [[
			var strum = game.strumLineNotes.members[]]--[[];
			
			for (camera in strum.cameras)
			{
				if (!camera.visible || !camera.exists || !strum.isOnScreen(camera))
					continue;

				game.dad.angle += 10;

				

				//strum.getScreenPosition(strum._point, camera).subtractPoint(strum.offset);

				//strum.drawComplex(camera);
				camera.drawPixels(strum._frame, strum.framePixels, strum._matrix, strum.colorTransform, strum.blend, strum.antialiasing, strum.shader);
			}
			
			
		]]
		--[[runHaxeCode(shitcode);
		setPropertyFromGroup('strumLineNotes', i, 'x', getPropertyFromGroup('strumLineNotes', i, 'x')+300)
	end

end]]--


function setupCamera()

	if useShaders then 
		initLuaShader('raymarch')
	end
	--initLuaShader('multiDraw') --it works but doesnt work well with z axis
	addHaxeLibrary('FlxColor')

	--runHaxeCode('FlxG.signals.postDraw.add(function() { game.callOnLuas("onDrawPost", []); });') --holy shit it actually works wtf, didnt really work for what i wanted but still cool that you can do that lol
	

	if layerNotesWithStrums then 
		runHaxeCode('game.remove(game.notes);')
	end
	
	runHaxeCode('game.strumLineNotes.visible = false;')
	runHaxeCode('game.strumLineNotes.active = false;')

	runHaxeCode('game.variables["ZLayeredStuffs"] = [ [ ] ];')

		runHaxeCode('game.variables["noteCamera"] = new FlxCamera();')
		runHaxeCode('game.variables["noteCamera"].bgColor = 0x00FFFFFF;')
		runHaxeCode('FlxG.cameras.add(game.variables["noteCamera"]);')
		if useShaders then 
			runHaxeCode('game.variables["raymarchShader"] = game.createRuntimeShader("raymarch");')
			runHaxeCode('game.variables["noteCamera"].setFilters([new ShaderFilter(game.variables["raymarchShader"])]);')
		end


		--runHaxeCode('game.variables["playfieldShader'..i..'"] = game.createRuntimeShader("multiDraw");')
		--runHaxeCode('game.variables["playfieldShader'..i..'"].setInt("playfieldCount", 2);')
		--runHaxeCode('game.variables["playfieldShader'..i..'"].setFloatArray("playfieldX", [(1280/2),0,0,0,0,0,0,0]);')
		--runHaxeCode('game.variables["playfieldShader'..i..'"].setFloatArray("playfieldY", [0,0,0,0,0,0,0,0]);')
		--runHaxeCode('game.variables["noteCamera'..i..'"].setFilters([new ShaderFilter(game.variables["playfieldShader'..i..'"])]);')

	runHaxeCode('FlxG.cameras.add(game.camOther);')

	for i = 0, getProperty('unspawnNotes.length')-1 do
		if getPropertyFromGroup('unspawnNotes', i, 'mustPress') then 
			runHaxeCode('game.unspawnNotes['..i..'].cameras = [game.variables["noteCamera"]];')
		else 
			runHaxeCode('game.unspawnNotes['..i..'].cameras = [game.variables["noteCamera"]];')
		end
	end

	for i = 0, (keyCount*2)-1 do
		if i < keyCount then 
			runHaxeCode('game.strumLineNotes.members['..i..'].cameras = [game.variables["noteCamera"]];')
		else 
			runHaxeCode('game.strumLineNotes.members['..i..'].cameras = [game.variables["noteCamera"]];')
		end
	end
end


function clamp(val, min, max)
	if val < min then
		val = min
	elseif max < val then
		val = max
	end
	return val
end
--https://stackoverflow.com/questions/5294955/how-to-scale-down-a-range-of-numbers-with-a-known-min-and-max-value
function scale(valueIn, baseMin, baseMax, limitMin, limitMax)
	return ((limitMax - limitMin) * (valueIn - baseMin) / (baseMax - baseMin)) + limitMin
end

function getNoteRot(data, curPos, XPos, YPos)
	local x = 0
	local y = 0
	local z = -1

	--fucking math
	local strumRotX = getCartesianCoords3D(getNoteRotX(data, curPos),90, XPos+(arrowSize/2)-(screenWidth/2))
	x = strumRotX[1]-(arrowSize/2)+(screenWidth/2)
	local strumRotY = getCartesianCoords3D(90,getNoteRotY(data, curPos), YPos+(arrowSize/2)-(screenHeight/2))
	y = strumRotY[2]-(arrowSize/2)+(screenHeight/2)
	--notePosY = _G['default'..strum..'Y'..i%keyCount]+strumRot[2]
	z = z + strumRotX[3] + strumRotY[3]
	return {x,y,z}
end

local zNear = 0
local zFar = 1000
local zRange = zNear - zFar 
local tanHalfFOV = math.tan(math.pi/4)

function calculatePerspective(x,y,z,offsetX)

	x = x - (screenWidth/2)
	y = y - (screenHeight/2)

	local zPerspectiveOffset = (z+(2 * zFar * zNear / zRange));

	x = x + (offsetX / (1/-zPerspectiveOffset)) --for sustains

	local xPerspective = x*(1/tanHalfFOV);
	local yPerspective = y/(1/tanHalfFOV);
	xPerspective = xPerspective/-zPerspectiveOffset;
	yPerspective = yPerspective/-zPerspectiveOffset;

	xPerspective = xPerspective + (screenWidth/2)
	yPerspective = yPerspective + (screenHeight/2)

	return {xPerspective,yPerspective,zPerspectiveOffset}

	--explaination on perspective math
	--to start you need some basic variables for far, near, and range and the fov (tan halfed), ive skipped the actual fov since its not really needed (its just math.pi/2, which is 90 deg)
	--[[
		local zNear = 0
		local zFar = 100
		local zRange = zNear - zFar 
		local tanHalfFOV = math.tan(math.pi/4)
	]]
	--next, you need the position of your object without any perspective applied
	--you then take away that position by the screen width/height / 2 (it will be added back later so down worry about it), 
	--this is so fake "3D" camera focuses on the center of the screen 

	--you then do the main calculation for the perspective which is 
	--[[
		(z+(2 * zFar * zNear / zRange));
	]]--
	--there are more but its not nessessary for this

	--now you can transform the x and y using the halftanfov and the calculated perspective,
	--[[
		local xPerspective = notePosX*(1/tanHalfFOV);
		local yPerspective = notePosY/(1/tanHalfFOV);
		xPerspective = xPerspective/-zPerspectiveOffset;
		yPerspective = yPerspective/-zPerspectiveOffset;
	]]--
	--now you just add the screen width/height / 2 back onto the x/y and thats it for the main positioning

	--the only thing left is to do the scaling
	--which is just multiplying the scale by (1/-zPerspectiveOffset)
	--you need to remember the default scale though, for notes its just 0.7
	--[[
		getProperty('scale.x') * (1/-zPerspectiveOffset)
		getProperty('scale.y') * (1/-zPerspectiveOffset)
	]]

	--good place to go if you wanna learn more about this kinda stuff https://ogldev.org/www/tutorial12/tutorial12.html
end

--the funny spherical to cartesian for incoming angles
function getCartesianCoords3D(theta, phi, radius)

	local x = 0
	local y = 0
	local z = 0

	
	x = math.cos(theta*rad)*math.sin(phi*rad);
	y = math.cos(phi*rad);
	z = math.sin(theta*rad)*math.sin(phi*rad);
	x = x*radius;
	y = y*radius;
	z = z*radius;

	return {x,y,z/1000}
end

function setupStrumPosition(i)
	local strum = 'PlayerStrum'
	if i < keyCount then 
		strum = 'OpponentStrum'
	end
	local notePosX = _G['default'..strum..'X'..i%keyCount]
	local notePosY = _G['default'..strum..'Y'..i%keyCount]
	local notePosZ = -1
	

	local noteRotPos = getNoteRot(i, 0, notePosX, notePosY)
	notePosX = noteRotPos[1]
	notePosY = noteRotPos[2]
	notePosZ = noteRotPos[3]

	local offsets = runModifiers(i, 0) --calc offsets
	notePosX = notePosX + offsets[1]
	notePosY = notePosY + offsets[2]
	notePosZ = notePosZ + offsets[3]
	local noteAngle = offsets[4]
	local targetAlpha = 1
	if i < keyCount then 
		if middlescroll then 
			targetAlpha = 0.35
		end
	end
	local noteAlpha = offsets[5] * targetAlpha

	local finalPos = calculatePerspective(notePosX, notePosY, notePosZ, 0)
	local zScale = finalPos[3]
	setPropertyFromGroup('strumLineNotes', i, 'x', finalPos[1])
	setPropertyFromGroup('strumLineNotes', i, 'y', finalPos[2])
	setPropertyFromGroup('strumLineNotes', i, 'angle', noteAngle)

	if songPosition > 0 then --for intro fade in
		setPropertyFromGroup('strumLineNotes', i, 'alpha', noteAlpha)
	end
	

	setPropertyFromGroup('strumLineNotes', i, 'scale.x', getScaleX(i, 0) * (1/-zScale))
	setPropertyFromGroup('strumLineNotes', i, 'scale.y', getScaleY(i, 0) * (1/-zScale))


	if not legacyPsychMode then
		runHaxeCode('game.variables["ZLayeredStuffs"].push([game.strumLineNotes.members['..i..'], '..notePosZ..']);')
	end
end 

function setupNotePosition(i)
	local mustPress = getPropertyFromGroup('notes', i, 'mustPress');
	local isSustainNote = getPropertyFromGroup('notes', i, 'isSustainNote');
	local noteData = getPropertyFromGroup('notes', i, 'noteData');
	local isSusEnd = string.find(string.lower(getPropertyFromGroup('notes', i, 'animation.curAnim.name')), 'end') or string.find(string.lower(getPropertyFromGroup('notes', i, 'animation.curAnim.name')), 'tail') --check tail in case playing on engine with extra keys

	local strumPos = noteData+keyCount
	local strum = 'PlayerStrum'
	if not mustPress then 
		strum = 'OpponentStrum'
		strumPos = noteData
	end

	local notePosX = _G['default'..strum..'X'..noteData%keyCount]
	local notePosY = _G['default'..strum..'Y'..noteData%keyCount]
	local notePosZ = -1

	local strumDiff = (songPosition-getPropertyFromGroup('notes', i, 'strumTime'))

	if mustPress then 

		--reused code from cancelled mod with time stop mechanic
		--if (timeStopEnd-timeStopStart) > 0 and getPropertyFromGroup('notes', i, 'strumTime') > timeStopEnd and timeStopStart ~= 0 then 
			--[[if songPosition >= timeStopStart then 
				strumDiff = timeStopEnd - getPropertyFromGroup('notes', i, 'strumTime')
			else 
				strumDiff = strumDiff + (timeStopEnd-timeStopStart)
			end]]--

		--end		
	end
	local timeBetweenNextSustain = (stepCrochet + (stepCrochet/songSpeed))
	local curPos = strumDiff * songSpeed
	if mustPress then 
		if (string.lower(songName) == 'combo meal' and songPosition > 54180 and songPosition < 93500) or (string.lower(songName) == 'combo meal erect' and songPosition > 41700 and songPosition < 82120)  then 
			if curPos <= -1250 then 
				curPos = -1250 + (curPos*0.02)
				setPropertyFromGroup('notes', i, 'colorSwap.saturation', -0.95)
				setPropertyFromGroup('notes', i, 'colorSwap.brightness', -0.2)
			elseif curPos <= -700 then 
				local a = (700-math.abs(curPos))/(700-1250)
				setPropertyFromGroup('notes', i, 'colorSwap.saturation', -0.95*a)
				setPropertyFromGroup('notes', i, 'colorSwap.brightness', -0.2*a)
			else
				setPropertyFromGroup('notes', i, 'colorSwap.saturation', 0)
				setPropertyFromGroup('notes', i, 'colorSwap.brightness', 0)
			end
		end	
	end
	local nextPos = curPos + timeBetweenNextSustain
	curPos = runPosModifiers(strumPos, curPos)
	nextPos = runPosModifiers(strumPos, nextPos)

	local noteRotPos = getNoteRot(strumPos, curPos, notePosX, notePosY)
	notePosX = noteRotPos[1]
	notePosY = noteRotPos[2]
	notePosZ = noteRotPos[3]

	local noteDist = getNoteDist(strumPos, curPos, 0.45)

	local fixedDist = noteDist
	if downscroll then 
		fixedDist = noteDist * -1 --flip for downscroll
	end

	local incomingAngleStuff = getCartesianCoords3D(getIncomingAngleX(strumPos, curPos),getIncomingAngleY(strumPos, curPos), curPos*fixedDist)
	notePosX = notePosX - incomingAngleStuff[1]
	notePosY = notePosY - incomingAngleStuff[2]
	notePosZ = notePosZ + incomingAngleStuff[3]

	if isSustainNote and fixedDist < 0 then --fix downscroll sustains, just psych engine code put into lua
		setPropertyFromGroup('notes', i, 'flipY', true)
		if isSusEnd then
			notePosY = notePosY + 10.5 * (fakeCrochet / 400) * 1.5 * songSpeed + (46 * (songSpeed - 1));
			notePosY = notePosY - (46 * (1 - (fakeCrochet / 600)) * songSpeed);
			if getPropertyFromClass('PlayState', 'isPixelStage') then
				notePosY = notePosY + 8 + (6 - getPropertyFromGroup('notes', i, 'originalHeightForCalcs')) * 6;
			else
				notePosY = notePosY - 19;
			end
			
		end
		notePosY = notePosY + ((arrowSize) / 2) - (60.5 * (songSpeed - 1));
		notePosY = notePosY + 27.5 * ((bpm / 100) - 1) * (songSpeed - 1);
	else 
		setPropertyFromGroup('notes', i, 'flipY', false)
	end

	local offsets = runModifiers(strumPos, curPos) --calc offsets
	notePosX = notePosX + offsets[1]
	notePosY = notePosY + offsets[2]
	notePosZ = notePosZ + offsets[3]
	local noteAngle = offsets[4] + getPropertyFromGroup('notes', i, 'offsetAngle')
	local targetAlpha = 1
	if strumPos < keyCount then 
		if middlescroll then 
			targetAlpha = 0.35
		end
	end
	local noteAlpha = offsets[5] * targetAlpha * getPropertyFromGroup('notes', i, 'multAlpha')

	local finalPos = calculatePerspective(notePosX,notePosY,notePosZ,getPropertyFromGroup('notes', i, 'offsetX'))
	notePosX = finalPos[1]
	notePosY = finalPos[2]
	local zScaleShit = finalPos[3]
	setPropertyFromGroup('notes', i, 'x', notePosX)
	setPropertyFromGroup('notes', i, 'y', notePosY)

	
	setPropertyFromGroup('notes', i, 'alpha', noteAlpha)


	setPropertyFromGroup('notes', i, 'distance', notePosZ) --just stealing this variable for z layering lol

	if not legacyPsychMode and layerNotesWithStrums then 
		runHaxeCode('game.variables["ZLayeredStuffs"].push([game.notes.members['..i..'], '..notePosZ..']);')
	end

	setPropertyFromGroup('notes', i, 'scale.x', getScaleX(strumPos, curPos) * (1/-zScaleShit))
	if not isSustainNote then 
		setPropertyFromGroup('notes', i, 'scale.y', getScaleY(strumPos, curPos) * (1/-zScaleShit))
		setPropertyFromGroup('notes', i, 'angle', noteAngle)
	else 
		if defaultSusEndScaleY == -1 and isSusEnd then 
			defaultSusEndScaleY = getPropertyFromGroup('notes', i, 'scale.y') --get sustain scaley
		
		elseif defaultSusScaleY == -1 and not isSusEnd then 
			defaultSusScaleY = getPropertyFromGroup('notes', i, 'scale.y') --get sustain scaley
		end

		if isSusEnd then --do sustains scale shit
			setPropertyFromGroup('notes', i, 'scale.y', defaultSusEndScaleY * (1/-zScaleShit) * math.abs(fixedDist/0.45) * math.abs(songSpeed/scrollSpeed) * ((nextPos-curPos)/timeBetweenNextSustain))
		else 
			setPropertyFromGroup('notes', i, 'scale.y', defaultSusScaleY * (1/-zScaleShit) * math.abs(fixedDist/0.45) * math.abs(songSpeed/scrollSpeed) * ((nextPos-curPos)/timeBetweenNextSustain))--* math.abs(fixedDist/0.45)
		end

		--setPropertyFromGroup('notes', i, 'height', math.abs(getPropertyFromGroup('notes', i, 'scale.y')) * getPropertyFromGroup('notes', i, 'frameHeight'))
		--setPropertyFromGroup('notes', i, 'offset.y', -0.5 * (getPropertyFromGroup('notes', i, 'height') - getPropertyFromGroup('notes', i, 'frameHeight')))

	end
end


function setupSustainAngleTest(i)

	runHaxeCode([[

		var idx = game.notes.members[]]..i..[[].parent.tail.indexOf(game.notes.members[]]..i..[[]);

		
		idx--;
		
		var nextNote = game.notes.members[idx+1];
		if (idx < 0)
		{
			idx = 0;
			nextNote = game.notes.members[]]..i..[[].parent;
		}
		nextNote = game.notes.members[idx];


			

		var nextNoteY = nextNote.y;
		var thisNoteY = game.notes.members[]]..i..[[].y;
		
		var nextNoteX = nextNote.x;
		var thisNoteX = game.notes.members[]]..i..[[].x;

		local ang = 0
		if (ClientPrefs.downScroll) 
			ang = FlxAngle.asDegrees(Math.atan2( (nextNoteY-thisNoteY), (nextNoteX-thisNoteX) ) - (Math.PI/2)); --calc angle by comparing difference in position
		else 
			ang = FlxAngle.asDegrees(Math.atan2( (nextNoteY-thisNoteY), (nextNoteX-thisNoteX) ) + (Math.PI/2));

		game.notes.members[]]..i..[[].angle = ang;
		
	]])
end

function setupSustainAngle(i)
	local mustPress = getPropertyFromGroup('notes', i, 'mustPress');
	local isSustainNote = getPropertyFromGroup('notes', i, 'isSustainNote');
	local noteData = getPropertyFromGroup('notes', i, 'noteData');

	---calc note before

	--work out angle by calculating where the next sustain note would be

	local strumDiff = (songPosition-getPropertyFromGroup('notes', i, 'strumTime'))

	if mustPress then 
		if (timeStopEnd-timeStopStart) > 0 and getPropertyFromGroup('notes', i, 'strumTime') > timeStopEnd and timeStopStart ~= 0 then 
			if songPosition >= timeStopStart then 
				strumDiff = timeStopEnd - getPropertyFromGroup('notes', i, 'strumTime')
			else 
				strumDiff = strumDiff + (timeStopEnd-timeStopStart)
			end
		end
	end

	local curPos = strumDiff * songSpeed

	local strumPos = noteData+keyCount
	local strum = 'PlayerStrum'
	if not mustPress then 
		strum = 'OpponentStrum'
		strumPos = noteData
	end
	
	local nextPos = curPos + (stepCrochet + (stepCrochet/songSpeed))
	nextPos = runPosModifiers(strumPos, nextPos)
	local nextNoteY = _G['default'..strum..'Y'..noteData%keyCount]
	local thisNoteY = getPropertyFromGroup('notes', i, 'y');
	
	local nextNoteX = _G['default'..strum..'X'..noteData%keyCount]
	local thisNoteX = getPropertyFromGroup('notes', i, 'x');
	local nextNoteZ = -1
	--local thisNoteZ = notePosZ

	local noteDist = getNoteDist(strumPos, nextPos, 0.45)
	local fixedDist = noteDist
	if downscroll then 
		fixedDist = noteDist * -1 --flip for downscroll
	end

	local nextNoteRot = getNoteRot(strumPos, nextPos, nextNoteX, nextNoteY)
	nextNoteX = nextNoteRot[1]
	nextNoteY = nextNoteRot[2]
	nextNoteZ = nextNoteRot[3]
	
	local nextNoteincomingAngleStuff = getCartesianCoords3D(getIncomingAngleX(strumPos, nextPos),getIncomingAngleY(strumPos, nextPos), nextPos*fixedDist)
	nextNoteX = nextNoteX - nextNoteincomingAngleStuff[1]
	nextNoteY = nextNoteY - nextNoteincomingAngleStuff[2]
	nextNoteZ = nextNoteZ + nextNoteincomingAngleStuff[3]


	local susOffsets = runModifiers(strumPos, nextPos) --do mods for fake sustain
	nextNoteX = nextNoteX + susOffsets[1]
	nextNoteY = nextNoteY + susOffsets[2]
	nextNoteZ = nextNoteZ + susOffsets[3]

	local finalNextPos = calculatePerspective(nextNoteX,nextNoteY,nextNoteZ,getPropertyFromGroup('notes', i, 'offsetX'))
	nextNoteX = finalNextPos[1]
	nextNoteY = finalNextPos[2]
	local ang = 0
	if fixedDist < 0 then 
		ang = math.deg(math.atan2( (nextNoteY-thisNoteY), (nextNoteX-thisNoteX) ) - (math.pi/2)) --calc angle by comparing difference in position
		--debugPrint(ang)
	else 
		ang = math.deg(math.atan2( (nextNoteY-thisNoteY), (nextNoteX-thisNoteX) ) + (math.pi/2))
	end

	--local angz = (nextNoteX * thisNoteX + nextNoteY * thisNoteY + nextNoteZ * thisNoteZ) / 
	--(math.sqrt((nextNoteX*nextNoteX) + (nextNoteY*nextNoteY) + (nextNoteZ*nextNoteZ)) * 
	--math.sqrt((thisNoteX*thisNoteX) + (thisNoteY*thisNoteY) + (thisNoteZ*thisNoteZ)))

	--https://www.omnicalculator.com/math/angle-between-two-vectors
	--α = arccos[(xa * xb + ya * yb + za * zb) / (√(xa2 + ya2 + za2) * √(xb2 + yb2 + zb2))]
	--math.acos(angz)
	setPropertyFromGroup('notes', i, 'angle', ang)
end



--basically the note positioning code copied lol
function RenderNotePaths(i)
	for j = 0, pathCount do 
		--local isSustainNote = getPropertyFromGroup('notes', i, 'isSustainNote');
		local noteData = i
		--local isSusEnd = string.find(string.lower(getPropertyFromGroup('notes', i, 'animation.curAnim.name')), 'end') or string.find(string.lower(getPropertyFromGroup('notes', i, 'animation.curAnim.name')), 'tail') --check tail in case playing on engine with extra keys
	
		local strumPos = i
		local strum = 'PlayerStrum'
		
		if i < keyCount then 
			strum = 'OpponentStrum'
			setProperty(i..'NotePath'..j..'.alpha', getProperty('opponentNotePathAlpha.alpha'))
		else 
			setProperty(i..'NotePath'..j..'.alpha', getProperty('playerNotePathAlpha.alpha'))
		end

		local notePosX = _G['default'..strum..'X'..noteData%keyCount]
		local notePosY = _G['default'..strum..'Y'..noteData%keyCount]
		local notePosZ = -1
		local noteAngle = 0

		local curPos = -pathSize*j/0.45
		curPos = runPosModifiers(strumPos, curPos)

		local noteRotPos = getNoteRot(strumPos, curPos, notePosX, notePosY)
		notePosX = noteRotPos[1]
		notePosY = noteRotPos[2]
		notePosZ = noteRotPos[3]

		local noteDist = getNoteDist(strumPos, curPos, 0.45)
	
		local fixedDist = noteDist
		if downscroll then 
			fixedDist = noteDist * -1 --flip for downscroll
		end

		local incomingAngleStuff = getCartesianCoords3D(getIncomingAngleX(strumPos, curPos),getIncomingAngleY(strumPos, curPos), curPos*fixedDist)
		notePosX = notePosX - incomingAngleStuff[1]
		notePosY = notePosY - incomingAngleStuff[2]
		notePosZ = notePosZ + incomingAngleStuff[3]

		local offsets = runModifiers(strumPos, curPos) --calc offsets
		notePosX = notePosX + offsets[1]
		notePosY = notePosY + offsets[2]
		notePosZ = notePosZ + offsets[3]
		noteAngle = noteAngle + offsets[4]

		local finalPos = calculatePerspective(notePosX,notePosY,notePosZ,37)
		notePosX = finalPos[1]
		notePosY = finalPos[2]
		local zScaleShit = finalPos[3]
		setProperty(i..'NotePath'..j..'.x', notePosX)
		setProperty(i..'NotePath'..j..'.y', notePosY)

		


		setProperty(i..'NotePath'..j..'.scale.x', 1*(1/-zScaleShit))
		setProperty(i..'NotePath'..j..'.scale.y', 1*(1/-zScaleShit))

		---calc note before

		--work out angle by calculating where the next sustain note would be
		local nextPos = curPos - (pathSize/0.45)
		local nextNoteY = _G['default'..strum..'Y'..noteData%keyCount]
		local thisNoteY = finalPos[2]
		
		local nextNoteX = _G['default'..strum..'X'..noteData%keyCount]
		local thisNoteX = finalPos[1]
		local nextNoteZ = -1
		local thisNoteZ = notePosZ

		local nextNoteRot = getNoteRot(strumPos, nextPos, nextNoteX, nextNoteY)
		nextNoteX = nextNoteRot[1]
		nextNoteY = nextNoteRot[2]
		nextNoteZ = nextNoteRot[3]
		
		local nextNoteincomingAngleStuff = getCartesianCoords3D(getIncomingAngleX(strumPos, nextPos),getIncomingAngleY(strumPos, nextPos), nextPos*fixedDist)
		nextNoteX = nextNoteX - nextNoteincomingAngleStuff[1]
		nextNoteY = nextNoteY - nextNoteincomingAngleStuff[2]
		nextNoteZ = nextNoteZ + nextNoteincomingAngleStuff[3]


		local susOffsets = runModifiers(strumPos, nextPos) --do mods for fake sustain
		nextNoteX = nextNoteX + susOffsets[1]
		nextNoteY = nextNoteY + susOffsets[2]
		nextNoteZ = nextNoteZ + susOffsets[3]

		local finalNextPos = calculatePerspective(nextNoteX,nextNoteY,nextNoteZ,37)
		nextNoteX = finalNextPos[1]
		nextNoteY = finalNextPos[2]
		local ang = 0
		if fixedDist < 0 then 
			ang = math.deg(math.atan2( (nextNoteY-thisNoteY), (nextNoteX-thisNoteX) ) - (math.pi/2)) --calc angle by comparing difference in position
			--debugPrint(ang)
		else 
			ang = math.deg(math.atan2( (nextNoteY-thisNoteY), (nextNoteX-thisNoteX) ) + (math.pi/2))
		end

		--local angz = (nextNoteX * thisNoteX + nextNoteY * thisNoteY + nextNoteZ * thisNoteZ) / 
		--(math.sqrt((nextNoteX*nextNoteX) + (nextNoteY*nextNoteY) + (nextNoteZ*nextNoteZ)) * 
		--math.sqrt((thisNoteX*thisNoteX) + (thisNoteY*thisNoteY) + (thisNoteZ*thisNoteZ)))

		--https://www.omnicalculator.com/math/angle-between-two-vectors
		--α = arccos[(xa * xb + ya * yb + za * zb) / (√(xa2 + ya2 + za2) * √(xb2 + yb2 + zb2))]
		--math.acos(angz)
		--setPropertyFromGroup('notes', i, 'angle', ang)
		setProperty(i..'NotePath'..j..'.angle', ang)
	end
end