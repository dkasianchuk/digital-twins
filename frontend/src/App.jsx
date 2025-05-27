import React, { useState, useRef, useEffect } from "react";
import Split from "react-split";
import CodeMirror from "@uiw/react-codemirror";
import { Decoration, ViewPlugin } from '@codemirror/view';
import './App.scss';
import UploadModal from './UploadModal';
import Tree from 'react-d3-tree';

const createHighlightPlugin = (from, to) =>
  ViewPlugin.fromClass(
    class {
      constructor() {
        this.decorations = Decoration.set([
          Decoration.mark({ class: 'highlightError' }).range(from, to+1),
        ]);
      }

      update(update) {
        if (update.docChanged || update.viewportChanged) {
          this.decorations = Decoration.set([
            Decoration.mark({ class: 'highlightError' }).range(from, to+1),
          ]);
        }
      }
    },
    {
      decorations: (v) => v.decorations,
    }
  );

  const tabs = [
    { id: 1, label: 'Node Info', context: 'info' },
    { id: 2, label: 'Errors', context:'errors' },
  ];

  const speeds = [0.25, 0.5, 1, 1.25, 1.5, 2];

function App() {
  const [isOpen, setIsOpen] = useState(true);
  const [code, setCode] = useState("");
  const [lang, setLang] = useState(null);
  const [relations, setRelations] = useState({})
  const [nodeData, setNodeData] = useState({})
  const [errors, setErrors] = useState([]);
  const [selectedError, setSelectedError] = useState(null);
  const [extensions, setExtensions] = useState([]);
  const [stepType, setStepType] = useState('simpleStep');
  const [stepCount, setStepCount] = useState(1);
  const [selectedNode, setSelectedNode] = useState(null);
  const [selectedNodes, setSelectedNodes] = useState({});
  const [disabledStepBtn, setDisabledStepBtn] = useState(true);
  const [activeTab, setActiveTab] = useState(tabs[0].id);
  const [initRule, setInitRule] = useState('');
  const [speed, setSpeed] = useState(1);
  const [translate, setTranslate] = useState({ x: 200, y: 50 });
  const treeContainer = useRef(null);

  const resetAll = () => {
    setRelations({});
    setNodeData({});
    setErrors([]);
    setSelectedError(null);
    setExtensions([]);
    setStepType('simpleStep');
    setStepCount(1);
    setSelectedNode(null);
    setSelectedNodes({});
    setDisabledStepBtn(true);
    setActiveTab(tabs[0].id);
    setInitRule('');
    setSpeed(1);
  };

  const contentActiveTab = tabs.find(tab => tab.id === activeTab)?.context

  const isCompleteNode = stepType === 'completeNodeStep';
  const isNodeDataEmpty = Object.keys(nodeData).length === 0;


  const ws = useRef(null);
  const nodeFrequency = useRef({});

  const normalizedTree = (id) => {
    const frequency = {}
    function helper(id, depth){
      const children = (relations[id] || []).map((child)=>helper(child, depth+1))
      const maxDepth = children.length ? Math.max(...children.map((child) => child.maxDepth)) : depth
      const value = nodeData[id].value
      frequency[value] = (frequency[value] || 0) + 1
      return {
        ...nodeData[id],
        depth,
        height: maxDepth - depth,
        maxDepth,
        children
      };
    }
    nodeFrequency.current = frequency;
    return helper(id, 0)
  };

  const tree = nodeData[1] ? normalizedTree(1) : {};
  
  const handleClick = (err) => {
    const plugin = createHighlightPlugin(err.startIndex, err.stopIndex);
    setExtensions([plugin]); 
    setSelectedError(err.id)
  };

  const handleNodeClick = (nodeData) => {
    const newSelectedNodes = {}
    const plugins = []
    function helper (nodeData) {
      newSelectedNodes[nodeData.id] = nodeData
      if(nodeData.type !== "node") {
        plugins.push(createHighlightPlugin(nodeData.startIndex, nodeData.stopIndex))
      }
      else {
        nodeData.children.forEach(helper)
      }
    }
    if(selectedNode && selectedNode.id === nodeData.id) {
      setSelectedNode(null)
    } else {
      helper(nodeData)
      setSelectedNode(nodeData)
    }
    setSelectedNodes(newSelectedNodes);
    setExtensions(plugins)
  };

  const sendMessage = (data) => {
    if (ws.current && ws.current.readyState === WebSocket.OPEN) {
      ws.current.send(JSON.stringify(data));
    }
  };

  useEffect(() => {
    const container = treeContainer.current;
    if (!container) return;

    const resizeObserver = new ResizeObserver((entries) => {
      for (let entry of entries) {
        const { width } = entry.contentRect;
        setTranslate({ x: width / 2, y: 100 });
      }
    });

    resizeObserver.observe(container);

    return () => resizeObserver.disconnect();
  }, []);

  useEffect(() => {
    ws.current = new WebSocket('ws://192.168.0.104:3000/v1/parse');

    ws.current.onopen = () => {
      console.log('WebSocket connected');
    };

    ws.current.onmessage = (event) => {
      const data = JSON.parse(event.data);
      switch(data.type){
        case 'stepEnd':
          setDisabledStepBtn(false);
          break;
        case 'parserError':
        case 'lexerError':
          setErrors(prevErrors => [data, ...prevErrors]);
          setActiveTab(2);
          handleClick(data)
          break;
        default:
            if (data.id) {
              const {parent, id} = data;
              if (!nodeData[id]) {
                setRelations(prevRelations => {
                  return {...prevRelations, [parent]: [...(prevRelations[parent] || []), id]}
                })
              }
              setNodeData((prevNodeData) => {
                return {...prevNodeData, [id]: {...(prevNodeData[id] || {}), ...data}}
              })
            }else if (data.uuid) {
              setDisabledStepBtn(false);
            }
      }
    };

    ws.current.onclose = () => {
      console.log('WebSocket disconnected');
    };

    return () => {
      ws.current.close();
    };
  }, []);

  const handleFileUpload = (event) => {
    const file = event.target.files[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = (e) => {
        const text = e.target.result;
        setCode(text);
      };
      reader.readAsText(file);
    }
  };

  const handleCodeChange = (value) => {
    setCode(value);
  };

  const handleStart = () => {
    resetAll();
    sendMessage({type: "init", lang, rule: initRule, "source": code});
  }

  const textLayout = {textAnchor: "small", x: 0, y: 0}
  const svgShape = {shape: 'circle', shapeProps: {r: 10}};

  const handleChangeStep = () => {
    let data = {type: stepType, timeout: 2000/speed}
    if(isCompleteNode) {
      data = {...data, id: selectedNode.id}
      setSelectedNode(null);
    }else {
      data = {...data, count: +stepCount}
    }
    sendMessage(data);
    setDisabledStepBtn(true);
  };
   
  const isShowInfoText = !selectedNode && isCompleteNode;

  const renderCustomNode = ({ nodeDatum, toggleNode }) => {
    const isSelected =
      selectedNode && selectedNodes[nodeDatum.id];
    
  
    return (
      <g
        onClick={() => {
          handleNodeClick(nodeDatum);
          setActiveTab(1);
          setSelectedError(null);
          toggleNode();
        }}
        style={{ cursor: 'pointer' }}
      >
        <circle r={15} fill={isSelected ? 'orange' : nodeDatum.processed ? nodeDatum.type === 'errorNode' ? 'orangered': 'lightgray' : 'lightblue'} strokeWidth='1px'/>
        <text fill="black" strokeWidth="0" x={20}>
          {nodeDatum.value}
        </text>
      </g>
    );
  };

  const handleExport = () => {
    const json = JSON.stringify(normalizedTree(selectedNode.id), null, 2); 
  
    const blob = new Blob([json], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
  
    const a = document.createElement('a');
    a.href = url;
    a.download = `${selectedNode.value}.json`;
    a.click();
  
    URL.revokeObjectURL(url);
  };
  
  


  return (
    <div className='visualizerWrapper'>
      <Split
        sizes={[50, 50]}
        minSize={100}
        gutterSize={3}
        gutterAlign="center"
        dragInterval={3}
        direction="horizontal"
        cursor="col-resize"
        className="spliter"
      >
        <Split
        sizes={[55, 45]}
        minSize={100}
        gutterSize={3}
        gutterAlign="center"
        dragInterval={3}
        direction="vertical"
        cursor="col-resize"
        className="spliterVertical"
        >
          <div className="codePart">
            <input type="file"  onChange={handleFileUpload} />
            <CodeMirror
              value={code}
              className="codeMirrorBox"
              extensions={extensions}
              onChange={(value) => handleCodeChange(value)}
            />  
            <div className="initBox">
            <input value={initRule} placeholder="Enter rule"  onChange={(e)=>setInitRule(e.target.value)}/>
            <button disabled={!initRule} onClick={handleStart}>Start</button>
            </div>
          </div>
          <div className="informTabs">
      <div className="tabsWrapper">
        {tabs.map(tab => (
          <button
            key={tab.id}
            onClick={() => setActiveTab(tab.id)}
            className="tabs"
            style={{
              borderBottom: activeTab === tab.id ? '2px solid orange' : 'none',
              fontWeight: activeTab === tab.id ? 'bold' : 'normal',
            }}
          >
            {`${tab.label} ${tab.label==='Errors' ? `(${errors.length})` : ''}`}
          </button>
        ))}
      </div>
      <div style={{ padding: '5px 20px' }}>
        {contentActiveTab === 'errors' &&  <ul className="errorsList">
            {errors.map((error) => <li className = {selectedError === error.id? 'highlightError' : ''} onClick={() => handleClick(error)}>{error.message}</li>)}
          </ul>}
        {
          contentActiveTab === 'info' && <div>
            <button disabled={!selectedNode} onClick={handleExport}>Save as JSON</button>
            {!selectedNode && <p>Please select node</p>}
            {selectedNode && <div>
              {selectedNode.type === "node" ? 
              <p>{`Rule name: ${selectedNode.value}`}</p> :
              <React.Fragment>
                <p>{`Rule name: ${selectedNode.name}`}</p>
                <p>{`Value: ${selectedNode.value}`}</p>
              </React.Fragment>}
              <p>{`Elapsed time: ${selectedNode.elapsedTime || '?'}`}</p>
              <p>{`Depth: ${selectedNode.depth}`}</p>
              <p>{`Height: ${selectedNode.height}`}</p>
              <p>{`Frequency: ${nodeFrequency.current[selectedNode.value]}`}</p>
              </div>}
          </div>
        }
      </div>
    </div>
         
        </Split>
        
        <div className="graphPart" ref={treeContainer}>
        <div className="stepBox">
        <select id="my-select" value={stepType} onChange={(event)=>setStepType(event.target.value)}>
        <option value="simpleStep">Simple Step</option>
        <option value="tokenStep">Token Step</option>
        <option disabled={isNodeDataEmpty} value="completeNodeStep">Complete Node</option>
        </select>
        <select id="select-speed" value={speed} onChange={(event)=>setSpeed(event.target.value)}>
          {
            speeds.map((time)=><option value={time}>{`${time}x`}</option>)
          }
        </select>
        {!isCompleteNode && <input value={stepCount}  onChange={(e)=>setStepCount(e.target.value)}/>}
        {isShowInfoText && <p>Please select node</p>}
        <button disabled={disabledStepBtn || (stepType==='completeNodeStep' && selectedNode?.processed)} onClick={handleChangeStep}>Step</button>
        </div>
          {!isNodeDataEmpty ? <Tree 
            data={tree} 
            collapsible={false}
            orientation="vertical"
            translate={translate}
            ransitionDuration={0}
            renderCustomNodeElement={(rd3tProps) => renderCustomNode(rd3tProps)}
            textLayout={textLayout}
            nodeSvgShape={svgShape}/>: null}
        </div>
      </Split>
      
      <UploadModal isOpen={isOpen} setIsOpen={setIsOpen} setLang={setLang}/>
    </div>
  )
}

export default App
