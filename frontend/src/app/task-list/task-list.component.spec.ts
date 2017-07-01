import { TestBed, async, inject } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpModule } from '@angular/http'
import { InfoMessagesService } from '../info-messages.service'

import { TaskListComponent } from './task-list.component';

describe('TaskListComponent', () => {

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [
                TaskListComponent
            ],
            imports: [
                RouterTestingModule,
                HttpModule
            ]
        }).compileComponents();
    }));

    it('should not have an <hr> element',  async(() => {
        const fixture = TestBed.createComponent(TaskListComponent);
        const nodes = fixture.debugElement.nativeElement;
        expect(nodes.querySelectorAll('hr').length).toEqual(0);
    }));
});
