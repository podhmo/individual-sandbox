/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */


import { Handle, Position, type NodeProps } from 'npm:@xyflow/react@12.4';

import { type PositionLoggerNode } from './nodes__types.ts';

export function PositionLoggerNode({
  positionAbsoluteX,
  positionAbsoluteY,
  data,
}: NodeProps<PositionLoggerNode>) {
  const x = `${Math.round(positionAbsoluteX)}px`;
  const y = `${Math.round(positionAbsoluteY)}px`;

  return (
    // We add this class to use the same styles as React Flow's default nodes.
    <div className="react-flow__node-default">
      {data.label && <div>{data.label}</div>}

      <div>
        {x} {y}
      </div>

      <Handle type="source" position={Position.Bottom} />
    </div>
  );
}
